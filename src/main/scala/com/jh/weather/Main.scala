package com.jh.weather

/**
 * For this assignment, I'm using a single file to express all of the various bits. This is generally not
 * best-practice (almost every type should have its own file), but I think for this particular task, it is
 * easier to read and follow if it's all just in-lime.
 *
 * I'm using the tagless final approach here (i.e. F[_]), which is total overkill and needlessly complicated for this
 * little service, but it does allow me to follow what I think is a best practice when working with the TypeLevel stack.
 * I've been of two minds with this approach, as it is a significant legibility barrier to newer devs. One could
 * certainly make the argument that the pain of the tagless final abstraction is more pain than benefit.
 */

import cats.data.{EitherT, Kleisli}
import cats.effect.{Async, ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.ember.server.*
import org.http4s.server.Server

import scala.Console

// both Latitude and Longitude are expressed as Case Classes which are just wrappers over floats. This
// pseudo strong-typing decreases the chance of transposing at Latitude for a Longitude and visa-versa.
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

object Logger:
  /** A console printing stand-in for a real logger
   */
  def apply[F[_]: Async] = new Logger[F] {
    def info(msg: String): F[Unit] = Async[F].delay(Console.println(s"INFO: ${msg}"))
    def error(msg: String): F[Unit] = Async[F].delay(Console.println(s"ERROR: ${msg}"))
  }

/**
 * Houses the environment-specific runtime configuration settings
 */
trait WeatherServiceConfig:
  val runtimeEnvironment: RuntimeEnvironment
  val nationalWeatherServiceUri: String
  val port: Int = 8080

object WeatherServiceConfig:

  /** Figures out the correct configuration for the given runtimeEnvironment
  */
  def fetchForRuntimeEnvironment(re: RuntimeEnvironment): Either[Throwable, WeatherServiceConfig] =
    re match
      case RuntimeEnvironment.Localhost =>
        new WeatherServiceConfig {
          val runtimeEnvironment: RuntimeEnvironment = re
          val nationalWeatherServiceUri: String = "todo"
        }.asRight
      case x =>
        Throwable(s"The environment '$x' does not have a config defined").asLeft

/**
 * An abstraction against the exterior HTTP service we will be calling to get weather data.
 */
trait NationalWeatherService[F[_]]:
  type ShortForecast = String
  def fetchShortForecast(latitude: Latitude, longitude: Longitude): ShortForecast

  /**
   * Names of metrics for this service
   */
  val metric_fetch_success = "nws_fetch_success"
  val metric_fetch_failure = "nws_fetch_failure"

object NationalWeatherService:
  def apply[F[_]: Async](
                          config: WeatherServiceConfig
                        )(using metrics: Metrics[F], logger: Logger[F]): NationalWeatherService[F] =
    new NationalWeatherService[F]:
      override def fetchShortForecast(latitude: Latitude, longitude: Longitude): ShortForecast = ???

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
  def apply[F[_]: Async](weatherServiceConfig: WeatherServiceConfig): Either[Throwable, Metrics[F]] =
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

  /** validation of the path args
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

  def apply[F[_] : Async](nws: NationalWeatherService[F])(using logger: Logger[F]) =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case _ @ GET -> Root / "forecast" / "short" / "lat" / LatitudeVar(latitude) / "lon" / LongitudeVar(longitude) =>
        Ok("asd")
    }.orNotFound

/**
 * The Component Harness is an abstraction across all of the components used by the system. This allows us to
 * spin up all of the components for a given environment (Dev, Test, Prod, etc) as the service spins up, housing
 * all of these environment-specific components in a single place. Thus, all instantiation is done in one go
 * when the service spins up.
 */
trait ComponentHarness[F[_]]:
  val configuration: WeatherServiceConfig
  val metrics: Metrics[F]
  val logger: Logger[F]
  val routes: Kleisli[F, org.http4s.Request[F], org.http4s.Response[F]]

  val nationalWeatherService: NationalWeatherService[F]

object ComponentHarness:
  def fetchComponentHarnessForEnvironment[F[_]: Async](
                                                        config: WeatherServiceConfig
                                                      ): Either[Throwable, ComponentHarness[F]] =
    config.runtimeEnvironment match
      case RuntimeEnvironment.Localhost =>
        for
          metricInstance <-  Metrics.apply[F](config)
        yield new ComponentHarness[F] {
          val configuration: WeatherServiceConfig = config
          val logger: Logger[F] = Logger.apply[F]
          val metrics = metricInstance

          given Logger[F] = logger
          given Metrics[F] = metrics

          val nationalWeatherService: NationalWeatherService[F] = NationalWeatherService.apply[F](config)
          val routes  = WeatherRoutes.apply(nationalWeatherService)
        }
      case x => Throwable(s"no ComponentHarness defined for env '$x'").asLeft


object Main extends IOApp:

  /** If this was written "for real" it would have the goo to figure out where it was spun up (local/dev/prod/etc)
   * so it could return the correct RuntimeEnvironment for where the service is currently running. In this assignment,
   * I'm just going to return Localhost.
   */
  private def fetchRuntimeEnvironment: Either[Throwable, RuntimeEnvironment] =
    RuntimeEnvironment.Localhost.asRight

  def run(args: List[String]): IO[ExitCode] =
    (for
      runtimeEnvironment <- EitherT.fromEither[IO](fetchRuntimeEnvironment)
      config <- EitherT.fromEither[IO](WeatherServiceConfig.fetchForRuntimeEnvironment(runtimeEnvironment))
      componentHarness <- EitherT.fromEither[IO](ComponentHarness.fetchComponentHarnessForEnvironment[IO](config))
    yield componentHarness)
      .value
      .flatMap:
          case Left(error) =>
            // failure mode--would need to know more about target env to write something that panics loudly
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
                 .withPort(port"8080")
                 .withHttpApp(componentHarness.routes)
                 .build
                _ <- Resource.eval(componentHarness.logger.info("server started! Awaiting inbound requests..."))
              yield server

            server
              .use { _ => IO.never }
              .as(ExitCode.Success)