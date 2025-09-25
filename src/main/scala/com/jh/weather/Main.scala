package com.jh.weather

/**
 * For this assignment, I'm using a single file to express all of the various bits (except for tests). This is
 * generally not best-practice (almost every type should have its own file), but I think for this particular task, it
 * is easier to read and follow if it's all just in-lime.
 *
 * I'm using the tagless final approach here (i.e. F[_]), which is total overkill and needlessly complicated for this
 * little service, but it does allow me to follow what I think is a best practice when working with the TypeLevel stack.
 * I've been of two minds with this approach, as it is a significant legibility barrier to newer devs. One could
 * certainly make the argument that the pain of the tagless final abstraction is more pain than benefit.
 */

import cats.data.{EitherT, Kleisli}
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.jh.weather
import io.circe.*
//import io.circe.derivation.{}
import io.circe.generic.semiauto.*
import io.circe.parser.decode
import org.http4s.circe.{accumulatingJsonOf, jsonEncoderOf}
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes}

// both Latitude and Longitude are expressed as Case Classes which are just wrappers over floats. This
// pseudo strong-typing decreases the chance of transposing at Latitude for a Longitude and visa-versa.
// note, these classes, if this was real, should have bounds checks so that only valid lat/lon values are accepted.
case class Latitude(value: Float)
case class Longitude(value: Float)

/**
 * The domain of all possible runtime environments
 */
enum RuntimeEnvironment:
  case Localhost, Development, Production

/**
 * an abstraction across an effectful, pure logger,
 */
trait Logger[F[_]]:
  def info(msg: String): F[Unit]
  def error(msg: String): F[Unit]
  def verbose(msg: String): F[Unit]

object Logger:
  /** A console printing stand-in for a real logger
   */
  def apply[F[_] : Async] = new Logger[F] {
    def info(msg: String): F[Unit] = Async[F].delay(Console.println(s"INFO: ${msg}"))
    def error(msg: String): F[Unit] = Async[F].delay(Console.println(s"ERROR: ${msg}"))
    def verbose(msg: String): F[Unit] = Async[F].delay(Console.println(s"VERBOSE: ${msg}"))
  }

/**
 * Houses the environment-specific runtime configuration settings
 */
trait WeatherServiceConfig:
  val runtimeEnvironment: RuntimeEnvironment
  val nationalWeatherServiceUri: String
  val port: Port = port"8080"

object WeatherServiceConfig:

  /** Figures out the correct configuration for the given runtimeEnvironment
   */
  def fetchForRuntimeEnvironment(re: RuntimeEnvironment): Either[Throwable, WeatherServiceConfig] =
    re match
      case RuntimeEnvironment.Localhost =>
        new WeatherServiceConfig {
          val runtimeEnvironment: RuntimeEnvironment = re

          // example: https://api.weather.gov/points/39.7456,-97.0892
          val nationalWeatherServiceUri: String = "https://api.weather.gov/points/"
        }.asRight
      case x =>
        Throwable(s"The environment '$x' does not have a config defined").asLeft

/**
 * An abstraction against the exterior HTTP service we will be calling to get weather data.
 */
trait NationalWeatherService[F[_]]:
  def fetchForecast(latitude: Latitude, longitude: Longitude): F[Either[Throwable, Forecast]]

object NationalWeatherService:

  /** return sub-property of call to NAS /points */
  case class NwsPointProperties(
                                 // this is the URI to pull the forecast, not the forecast itself
                                 forecast: String
                               )

  /** return type of call to NAS /points */
  case class NwsPoint(
                       properties: NwsPointProperties
                     )

  given Decoder[NwsPoint] = deriveDecoder[NwsPoint]

  object NwsPoint:
    def fromJson(str: String): Either[Throwable, NwsPoint] = decode[NwsPoint](str).leftMap(
      e => Throwable(s"could not parse JSON: ${e.getMessage}")
    )

  case class NwsForecastPropertyPeriod(
                                        number: Int,
                                        temperature: Float,
                                        shortForecast: String
                                      )

  case class NwsForecastProperties(
                                    periods: List[NwsForecastPropertyPeriod]
                                  )

  case class NwsForecast(
                          properties: NwsForecastProperties
                        ):

    val periodOne: Option[NwsForecastPropertyPeriod] =
      this.properties.periods.find(_.number == 1)

  object NwsForecast:
    def fromJson(str: String): Either[Throwable, NwsForecast] = decode[NwsForecast](str).leftMap(t =>
      Throwable(s"error deserializing NWS Forecast JSON: ${t.getMessage}")
    )

  /**
   * Names of metrics for this service
   */
  val metric_fetch_success = "nws_fetch_success"
  val metric_fetch_failure = "nws_fetch_failure"

  given Decoder[NwsPointProperties] = deriveDecoder[NwsPointProperties]
  given Decoder[NwsForecastPropertyPeriod] = deriveDecoder[NwsForecastPropertyPeriod]
  given Decoder[NwsForecastProperties] = deriveDecoder[NwsForecastProperties]
  given Decoder[NwsForecast] = deriveDecoder[NwsForecast]

  def apply[F[_] : Async](
                           config: WeatherServiceConfig
                         )(using metrics: Metrics[F], logger: Logger[F]): F[NationalWeatherService[F]] =

    // set up the automagic JSON -> Type converters
    given EntityDecoder[F, NwsPoint] = accumulatingJsonOf[F, NwsPoint]
    given EntityDecoder[F, NwsForecast] = accumulatingJsonOf[F, NwsForecast]

    EmberClientBuilder
      .default[F]
      .build
      .use { httpClient =>
        Async[F].pure {
          new NationalWeatherService[F]:
            override def fetchForecast(latitude: Latitude, longitude: Longitude): F[Either[Throwable, Forecast]] =
              val pointUri = config.nationalWeatherServiceUri + s"${latitude.value},${longitude.value}"

              (for
                _ <- logger.verbose(s"pulling NWS weather at point '$pointUri'").attemptT
                // this call returns back the URI we use to pull the actual forecast
                point <- httpClient.expect[NwsPoint](pointUri).attemptT
                // use the derived forecast URI to pull the forecast
                nwsForecast <- httpClient.expect[NwsForecast](point.properties.forecast).attemptT
                _ <- logger.verbose(s"successfully pulled forecast for '$pointUri'").attemptT
                _ <- metrics.uptick(metric_fetch_success).attemptT
                jhForecast <- EitherT.fromEither[F](Forecast.fromNws(nwsForecast))
              yield jhForecast)
                .onError { error =>
                  for
                    _ <- logger.error(s"failed pulling forecast: ${error.getMessage}").attemptT
                    _ <- metrics.uptick(metric_fetch_failure).attemptT
                    e <- EitherT.leftT[F, Forecast](error)
                  yield e
                }.value
        }
      }

/** return type from our service call */
case class Forecast(characterization: Forecast.ForecastCharacterization, shortForecast: Forecast.ShortForecast)

object Forecast:

  import NationalWeatherService.*
  type ShortForecast = String
  type Fahrenheit = Float

  /** this is used to boil down temperature to a 'characterization' */
  enum ForecastCharacterization(val value: String):
    case Frigid extends ForecastCharacterization("frigid")
    case Cold extends ForecastCharacterization("cold")
    case Brisk extends ForecastCharacterization("brisk")
    case Mild extends ForecastCharacterization("mild")
    case Warm extends ForecastCharacterization("warm")
    case Hot extends ForecastCharacterization("hot")
    case Inferno extends ForecastCharacterization("inferno")

  object ForecastCharacterization:
    def fromTemperatureF(f: Fahrenheit): ForecastCharacterization = f match
      case x if x < 0.0 => Frigid
      case x if x >= 0 && x < 40 => Cold
      case x if x >= 40 && x < 60 => Brisk
      case x if x >= 60 && x < 70 => Mild
      case x if x >= 70 && x < 85 => Warm
      case x if x >= 85 && x < 110 => Hot
      case _ => Inferno

    def fromString(str: String): Either[Throwable, ForecastCharacterization] = str match
      case Frigid.value => Frigid.asRight
      case Cold.value => Cold.asRight
      case Brisk.value => Brisk.asRight
      case Mild.value => Mild.asRight
      case Warm.value => Warm.asRight
      case Hot.value => Hot.asRight
      case Inferno.value => Inferno.asRight
      case x => Throwable(s"the value '$x' is not parsable to ForecastCharacterization").asLeft

  def fromNws(nws: NwsForecast): Either[Throwable, Forecast] =
      for
        perioedOne <- Either.fromOption(
          nws.periodOne,
          Throwable("no period 1 found in NWS forecast")
        )
        characterization <- Either.catchNonFatal(ForecastCharacterization.fromTemperatureF(perioedOne.temperature))
      yield Forecast(characterization, perioedOne.shortForecast)

  given Encoder[ForecastCharacterization] = new Encoder[ForecastCharacterization]:
    override def apply(a: ForecastCharacterization): Json = Json.fromString(a.value)

  given Decoder[ForecastCharacterization] = deriveDecoder[ForecastCharacterization]

  given Encoder[Forecast] = deriveEncoder[Forecast]

  given Decoder[Forecast] = deriveDecoder[Forecast]

/**
 * An abstraction against service monitoring. This would be realized with whatever metric infra was in use,
 * such as Prometheus.
 */
trait Metrics[F[_]]:
  /** Upticks by one the metric name given so we can follow trend lines in a tool like DataDog of whatnot
   */
  def uptick(metricName: String): F[Unit]

object Metrics:
  /**
   * constructor. If real, would figure out how to spin itself up for the given runtimeEnvironment. Here, I'm just
   * giving back a fake demonstrate how it would work.
   */
  def apply[F[_] : Async](weatherServiceConfig: WeatherServiceConfig): Either[Throwable, Metrics[F]] =
    weatherServiceConfig.runtimeEnvironment match
      case RuntimeEnvironment.Localhost =>
        new Metrics[F] {
          def uptick(metricName: String): F[Unit] =
            Async[F].delay(
              // of course, this would be real metric code, but we are faking it here...
              println(s"fake metric uptick for '$metricName'")
            )
        }.asRight
      case x => Throwable(s"no metrics defined for '$x'").asLeft

object WeatherRoutes:

  /** validation of path args
   */
  object FloatVar:
    def unapply(str: String): Option[Float] = str.toLowerCase match
      case e if e.isEmpty => None
      case s =>
        Either
          .catchNonFatal(str.toFloat)
          .toOption

  object LatitudeVar:
    def unapply(str: String): Option[Latitude] =
      FloatVar.unapply(str).map(Latitude.apply)

  object LongitudeVar:
    def unapply(str: String): Option[Longitude] =
      FloatVar.unapply(str).map(Longitude.apply)

  def apply[F[_] : Async](nws: F[NationalWeatherService[F]])(using logger: Logger[F], metrics: Metrics[F]) =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    given EntityEncoder[F, Forecast] = jsonEncoderOf[Forecast]

    HttpRoutes.of[F] {
      case _@GET -> Root / "forecast" / "short" / "lat" / LatitudeVar(latitude) / "lon" / LongitudeVar(longitude) =>
        (for
          _ <- EitherT.rightT[F, Throwable](logger.info(s"fetching lat $latitude, lon $longitude from NWS"))
          weatherService <- nws.attemptT
          forecast <- EitherT(weatherService.fetchForecast(latitude, longitude))
        yield forecast)
          .value
          .flatMap:
            case Left(error) =>
              for
                _ <- logger.error(s"NFS error: ${error.getMessage}")
                // this response should follow some sort of convention such that clients can get back a JSON
                // payload with some decent error messaging... This HTTP text does not play very nice with clients.
                resp <- InternalServerError("could not process request")
              yield resp
            case Right(forecast) => Ok().map(_.withEntity(forecast))
    }.orNotFound

/**
 * The Component Harness is an abstraction across all of the components used by the system. This allows us to
 * spin up all of the components for a given environment (Dev, Test, Prod, etc) as the service spins up, housing
 * all of these environment-specific components in a single place.
 */
trait ComponentHarness[F[_]]:
  val configuration: WeatherServiceConfig
  val metrics: Metrics[F]
  val logger: Logger[F]
  val routes: Kleisli[F, org.http4s.Request[F], org.http4s.Response[F]]
  val nationalWeatherService: F[NationalWeatherService[F]]

object ComponentHarness:
  def fetchComponentHarnessForEnvironment[F[_] : Async](
                                                         config: WeatherServiceConfig
                                                       ): Either[Throwable, ComponentHarness[F]] =
    config.runtimeEnvironment match
      case RuntimeEnvironment.Localhost =>
        for
          metricInstance <- Metrics.apply[F](config)
        yield new ComponentHarness[F] {
          val configuration: WeatherServiceConfig = config
          val logger: Logger[F] = Logger.apply[F]
          val metrics = metricInstance

          given Logger[F] = logger

          given Metrics[F] = metrics

          val nationalWeatherService: F[NationalWeatherService[F]] = NationalWeatherService.apply[F](config)
          val routes = WeatherRoutes.apply(nationalWeatherService)
        }
      case x => Throwable(s"no ComponentHarness defined for env '$x'").asLeft


object Main extends IOApp:

  /** If this was written "for real" it would have the goo to figure out where it was spun up (local/dev/prod/etc)
   * so it could return the correct RuntimeEnvironment for where the service is currently running. In this assignment,
   * I'm just going to return Localhost.
   */
  private def fetchRuntimeEnvironment: Either[Throwable, RuntimeEnvironment] =
    RuntimeEnvironment.Localhost.asRight

  /**
   * Entrypoint
   */
  def run(args: List[String]): IO[ExitCode] =
    (for
      runtimeEnvironment <- EitherT.fromEither[IO](fetchRuntimeEnvironment)
      config <- EitherT.fromEither[IO](WeatherServiceConfig.fetchForRuntimeEnvironment(runtimeEnvironment))
      componentHarness <- EitherT.fromEither[IO](ComponentHarness.fetchComponentHarnessForEnvironment[IO](config))
    yield componentHarness)
      .value
      .flatMap:
        case Left(error) =>
          // start up failure mode--would need to know more about target env to write something that panics loudly,
          // or perhaps not if there was infra tooling that would handle that.
          val logger = Logger.apply[IO]
          for
            _ <- logger.error(s"Failed to start: ${error.getMessage}")
          yield ExitCode.Error
        case Right(componentHarness) =>
          val server: Resource[IO, Server] =
            for
              _ <- Resource.eval(componentHarness.logger.info("starting service..."))
              server <- EmberServerBuilder
                .default[IO]
                .withHost(ipv4"0.0.0.0")
                .withPort(componentHarness.configuration.port)
                .withHttpApp(componentHarness.routes)
                .build
              _ <- Resource.eval(componentHarness.logger.info("server started! Awaiting inbound requests..."))
            yield server

          server
            .use { _ => IO.never }
            .as(ExitCode.Success)