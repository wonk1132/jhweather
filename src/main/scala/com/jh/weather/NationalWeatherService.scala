package com.jh.weather

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all.*
import com.jh.weather.*
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import org.http4s.EntityDecoder
import org.http4s.circe.accumulatingJsonOf
import org.http4s.ember.client.EmberClientBuilder

/** return sub-property of call to NAS /points */
case class NwsPointProperties(
  // this is the URI to pull the forecast, not the forecast itself
  forecast: String
)

/** return type of call to NAS /points */
case class NwsPoint(
  properties: NwsPointProperties
)

object NwsPoint:
  def fromJson(str: String): Either[Throwable, NwsPoint] =
    decode[NwsPoint](str).leftMap(e => Throwable(s"could not parse JSON: ${e.getMessage}"))

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
  /** the forecast from NWS has many periods, we are just defaulting to the first one, the most
    * near-term
    */
  val periodOne: Option[NwsForecastPropertyPeriod] =
    this.properties.periods.find(_.number == 1)

object NwsForecast:
  def fromJson(str: String): Either[Throwable, NwsForecast] =
    decode[NwsForecast](str).leftMap(t =>
      Throwable(s"error deserializing NWS Forecast JSON: ${t.getMessage}")
    )

given Decoder[NwsPointProperties] = deriveDecoder[NwsPointProperties]
given Decoder[NwsForecastPropertyPeriod] = deriveDecoder[NwsForecastPropertyPeriod]
given Decoder[NwsForecastProperties] = deriveDecoder[NwsForecastProperties]
given Decoder[NwsForecast] = deriveDecoder[NwsForecast]
given Decoder[NwsPoint] = deriveDecoder[NwsPoint]

/** An abstraction against the exterior HTTP service we will be calling to get weather data.
  */
trait NationalWeatherService[F[_]]:
  def fetchForecast(latitude: Latitude, longitude: Longitude): F[Either[Throwable, Forecast]]

object NationalWeatherService:

  /** Names of metrics for this service
    */
  val metric_fetch_success = "nws_fetch_success"
  val metric_fetch_failure = "nws_fetch_failure"

  def apply[F[_]: Async](
    config: WeatherServiceConfig
  )(using metrics: Metrics[F], logger: Logger[F]): F[NationalWeatherService[F]] =

    given EntityDecoder[F, NwsPoint] = accumulatingJsonOf[F, NwsPoint]
    given EntityDecoder[F, NwsForecast] = accumulatingJsonOf[F, NwsForecast]

    // since this is the only thing using a HTTP client, I just instantiate it here. This would be a prime
    // candidate to move to our ComponentHarness and passed in via dependency injection should other components
    // also need a HTTP client.
    EmberClientBuilder
      .default[F]
      .build
      .use { httpClient =>
        Async[F].pure {
          new NationalWeatherService[F]:
            override def fetchForecast(latitude: Latitude, longitude: Longitude)
              : F[Either[Throwable, Forecast]] =
              val pointUri =
                config.nationalWeatherServiceUri + s"${latitude.value},${longitude.value}"

              (for
                _ <- logger.verbose(s"pulling NWS weather at point '$pointUri'").attemptT
                // this call returns back the URI we use to pull the actual forecast
                point <- httpClient.expect[NwsPoint](pointUri).attemptT
                // use the derived forecast URI to pull the forecast
                nwsForecast <- httpClient.expect[NwsForecast](point.properties.forecast).attemptT
                _ <- logger.verbose(s"successfully pulled forecast for '$pointUri'").attemptT

                // give visibility of the service doing work so it can be monitored
                _ <- metrics.uptick(metric_fetch_success).attemptT
                // convert the complicated NWS forecast to our simplified two-property return type
                jhForecast <- EitherT.fromEither[F](Forecast.fromNws(nwsForecast))
              yield jhForecast).onError { error =>
                for
                  _ <- logger.error(s"failed pulling forecast: ${error.getMessage}").attemptT
                  _ <- metrics.uptick(metric_fetch_failure).attemptT
                  e <- EitherT.leftT[F, Forecast](error)
                yield e: Unit
              }.value
        }
      }
