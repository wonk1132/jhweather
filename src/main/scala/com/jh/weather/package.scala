package com.jh

import cats.effect.Async
import cats.syntax.all.*
import com.comcast.ip4s.*
import io.circe.*
import io.circe.Decoder.Result
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode

package object weather:

  type Fahrenheit = Float

  // both Latitude and Longitude are expressed as Case Classes which are just wrappers over floats. This
  // pseudo strong-typing decreases the chance of transposing at Latitude for a Longitude and visa-versa.
  // note, these classes, if this was real, should have bounds checks so that only valid lat/lon values are accepted.
  case class Latitude(value: Float)
  case class Longitude(value: Float)

  /** The domain of all possible runtime environments
    */
  enum RuntimeEnvironment:
    case Localhost, Development, Production

  /** an abstraction across an effectful, pure logger,
    */
  trait Logger[F[_]]:
    def info(msg: String): F[Unit]
    def error(msg: String): F[Unit]
    def verbose(msg: String): F[Unit]

  /** A console printing stand-in for a real logger
    */
  object Logger:
    def apply[F[_]: Async] = new Logger[F]:
      def info(msg: String): F[Unit] = Async[F].delay(Console.println(s"INFO: ${msg}"))

      def error(msg: String): F[Unit] = Async[F].delay(Console.println(s"ERROR: ${msg}"))

      def verbose(msg: String): F[Unit] = Async[F].delay(Console.println(s"VERBOSE: ${msg}"))

  /** Houses the environment-specific runtime configuration settings
    */
  trait WeatherServiceConfig:
    val runtimeEnvironment: RuntimeEnvironment
    val nationalWeatherServiceUri: String
    val port: Port = port"8080"

  object WeatherServiceConfig:

    /** Figures out the correct configuration for the given runtimeEnvironment
      */
    def fetchForRuntimeEnvironment(
      re: RuntimeEnvironment
    ): Either[Throwable, WeatherServiceConfig] =
      re match
        case RuntimeEnvironment.Localhost =>
          {
            new WeatherServiceConfig:
              val runtimeEnvironment: RuntimeEnvironment = re
              // example: https://api.weather.gov/points/39.7456,-97.0892
              val nationalWeatherServiceUri = "https://api.weather.gov/points/"
          }.asRight
        case x =>
          Throwable(s"The environment '$x' does not have a config defined").asLeft

  /** this is used to boil down temperature to a 'characterization' */
  enum ForecastCharacterization(val value: String):
    case Cold extends ForecastCharacterization("cold")
    case Moderate extends ForecastCharacterization("moderate")
    case Hot extends ForecastCharacterization("hot")

  object ForecastCharacterization:
    def fromTemperatureF(f: Fahrenheit): ForecastCharacterization = f match
      case x if x < 45.0 => Cold
      case x if x >= 45.0 && x < 82 => Moderate
      case _ => Hot

    def fromString(str: String): Either[Throwable, ForecastCharacterization] = str match
      case Cold.value => Cold.asRight
      case Moderate.value => Moderate.asRight
      case Hot.value => Hot.asRight
      case x => Throwable(s"the value '$x' is not parsable to ForecastCharacterization").asLeft

  /** return type from our service call, boiling down the complex NWS forecast
    */
  case class Forecast(
    characterization: ForecastCharacterization,
    shortForecast: String
  )

  object Forecast:
    def fromNws(nws: NwsForecast): Either[Throwable, Forecast] =
      for
        // the NWS forecast returns back several periods. Take the first one.
        periodOne <- Either.fromOption(
          nws.periodOne,
          Throwable("no period 1 found in NWS forecast")
        )
        characterization <- Either.catchNonFatal(
          ForecastCharacterization.fromTemperatureF(periodOne.temperature)
        )
      yield Forecast(characterization, periodOne.shortForecast)

    def fromJsonString(str: String): Either[Throwable, Forecast] = decode[Forecast](str).leftMap {
      error =>
        Throwable(error.getMessage)
    }

    given Encoder[ForecastCharacterization] = new Encoder[ForecastCharacterization]:
      override def apply(a: ForecastCharacterization): Json = Json.fromString(a.value)

    given Decoder[ForecastCharacterization] = new Decoder[ForecastCharacterization]:
      override def apply(c: HCursor): Result[ForecastCharacterization] =
        for
          f <- c.value.as[String]
          fc <- ForecastCharacterization
            .fromString(f)
            .leftMap(e => DecodingFailure(e.getMessage, c.history))
        yield fc

    given Encoder[Forecast] = deriveEncoder[Forecast]

    given Decoder[Forecast] = deriveDecoder[Forecast]

  /** An abstraction against service monitoring. This would be realized with whatever metric infra
    * was in use, such as Prometheus.
    */
  trait Metrics[F[_]]:
    /** Upticks by one the metric name given so we can follow trend lines in a tool like DataDog of
      * whatnot
      */
    def uptick(metricName: String): F[Unit]

  object Metrics:
    /** constructor. If real, would figure out how to spin itself up for the given
      * runtimeEnvironment. Here, I'm just giving back a fake demonstrate how it would work.
      */
    def apply[F[_]: Async](
      weatherServiceConfig: WeatherServiceConfig
    ): Either[Throwable, Metrics[F]] =
      weatherServiceConfig.runtimeEnvironment match
        case RuntimeEnvironment.Localhost =>
          new Metrics[F]:
            def uptick(metricName: String): F[Unit] =
              Async[F].delay(
                // of course, this would be real metric code, but we are faking it here...
                println(s"fake metric uptick for '$metricName'")
              )
          .asRight
        case x => Throwable(s"no metrics defined for '$x'").asLeft
