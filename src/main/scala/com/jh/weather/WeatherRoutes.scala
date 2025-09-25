package com.jh.weather

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all.*
import org.http4s.{EntityEncoder, HttpRoutes}
import org.http4s.circe.jsonEncoderOf
import org.http4s.dsl.Http4sDsl

/** The endpoints for the web server
  */
object WeatherRoutes:

  /** validation of path args. This validation is weak, it should protect against invalid lat/lon
    * values, supplying a better response to the client if they exceed the geographic bounds.
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

  def apply[F[_]: Async](
    nws: F[NationalWeatherService[F]]
  )(using logger: Logger[F], metrics: Metrics[F]) =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    given EntityEncoder[F, Forecast] = jsonEncoderOf[Forecast]

    HttpRoutes
      .of[F] {
        case _ @GET -> Root / "forecast" / "short" / "lat" /
            LatitudeVar(latitude) / "lon" / LongitudeVar(longitude) =>
          (for
            _ <- EitherT.rightT[F, Throwable](
              logger.info(s"fetching lat $latitude, lon $longitude from NWS")
            )
            weatherService <- nws.attemptT

            // there is no bounds-limits on lat/lon, which would need to be done if this was real. As it stands,
            // it would accept any float which could be totally nonsensical (i.e. 100000.3333) or invalid at NWS
            // (NWS will 404 for lat/lon of which it has no data (i.e. 100.0/100.0)). Adding bounds validation
            // could be a lot more ergonomic for clients--all we do now is 404, which is confusing.
            forecast <- EitherT(weatherService.fetchForecast(latitude, longitude))
          yield forecast).value
            .flatMap:
              case Left(error) =>
                for
                  _ <- logger.error(s"NFS error: ${error.getMessage}")
                  // this response should follow some sort of convention such that clients can get back a JSON
                  // payload with some decent error messaging... This HTTP text does not play very nice with clients.
                  resp <- InternalServerError("could not process request")
                yield resp
              case Right(forecast) => Ok().map(_.withEntity(forecast))
      }
      .orNotFound
