package com.jh.weather

import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.implicits.*
import com.jh.weather.ForecastCharacterization
import fs2.text
import org.http4s.circe.jsonEncoderOf
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.*

/**
 * typically, we would have a one-test-suite-class per class that we are testing (i.e the service, routes, etc).
 * I'm shortcutting here for readability.
 */
class EverythingSpec extends AnyFunSuite:

  val config: WeatherServiceConfig = WeatherServiceConfig
    .fetchForRuntimeEnvironment(RuntimeEnvironment.Localhost)
    .getOrElse(throw Throwable("could not decipher config for Localhost!"))

  given Logger[IO] = Logger.apply[IO]

  given Metrics[IO] = Metrics
    .apply[IO](config)
    .getOrElse(throw Throwable("could not spin up metrics"))

  given EntityEncoder[IO, Forecast] = jsonEncoderOf[Forecast]

  val client: IO[NationalWeatherService[IO]] = NationalWeatherService.apply[IO](config)

  // a fake NWS such that we have a stable component that gives back data we can control
  val fakeNwsService: IO[NationalWeatherService[IO]] = IO.pure {
    new NationalWeatherService[IO]:
      def fetchForecast(latitude: Latitude, longitude: Longitude): IO[Either[Throwable, Forecast]] =
        IO.pure {
          Forecast(
            ForecastCharacterization.Moderate,
            "foobar"
          ).asRight
        }
  }

  test("deserialiation of nws point json succeeds") {

    // this is just an optimistic test. Should add tests to ensure error when JSON is malformed or changed as well
    NwsPoint
      .fromJson(TestData.pointJson)
      .bimap(
        error => throw error,
        point =>
          point.properties.forecast.mustBe("https://api.weather.gov/gridpoints/TOP/32,81/forecast")
      )
  }

  test("deser of nws point fails") {
    // failure test case would be written here...
  }

  test("deser of nws forecast json succeeds") {
    NwsForecast
      .fromJson(TestData.forecastJson)
      .bimap(
        error => throw error,
        forecast =>
          val periodOne = forecast.properties.periods
            .find(_.number == 1)
            .getOrElse(throw Throwable("period 1 not found"))

          periodOne.temperature.mustBe(74)
          periodOne.shortForecast.mustBe("Mostly Sunny")
      )
  }

  test("deser of nws forecast json fails") {
    // failure test case would be written here...
  }

  test("NWS forecast results in correct JH Forecast") {
    val nwsForecast =
      NwsForecast.fromJson(TestData.forecastJson).getOrElse(throw Throwable("forecast deser error"))
    Forecast
      .fromNws(nwsForecast)
      .bimap(
        error => throw error,
        (forecast: Forecast) =>
          forecast.characterization.mustBe(ForecastCharacterization.Moderate)
          forecast.shortForecast.mustBe("Mostly Sunny")
      )

    // I can't see here how we could make a failure test case that isn't just redundant with the existing serdes test.
  }

  test("Forecast serdes") {
    val f = Forecast(ForecastCharacterization.Hot, "foobar")
    val dehydrated = f.asJson.noSpaces
    val rehydrated = Forecast.fromJsonString(dehydrated)
    rehydrated.mustBe(Right(f))

    Forecast
      .fromJsonString("""{"foo":"bar"}""")
      .isLeft.mustBe(true)
  }

  test("call to JH weather service succeeds") {

    val route = WeatherRoutes.apply(fakeNwsService)
    val forecastRequest = Request[IO](Method.GET, uri"forecast/short/lat/39.3/lon/-97.08")

    (for
      response <- route.run(forecastRequest)
      body <- response.body.through(text.utf8.decode).compile.string
      forecast = Forecast.fromJsonString(body)
      f = forecast.getOrElse(fail(s"no forecast found? $forecast"))
      _ = f.shortForecast.mustBe("foobar")
      _ = f.characterization.mustBe(ForecastCharacterization.Moderate)
    yield forecast).unsafeRunSync()
  }

  test("call to JH weather service fails with bad URI") {
    val route = WeatherRoutes.apply(fakeNwsService)
    val forecastRequest = Request[IO](Method.GET, uri"forecast/short/lat/39.3/foobar/-97.08")

    (for
      response <- route.run(forecastRequest)
      _ = response.status.code.mustBe(404)
    yield ()).unsafeRunSync()
  }

  // these IT tests should be in an IT suite, not in a unit test suite. Unit tests should be 100% deterministic,
  // but with these IT's we are testing our call to the real NWS service which will not return back stable data.
  // Normally, we would split these into in IT suite themselves, but I think that would be overkill for this example.
  test("IT test: pull forecast from NWS succeeds") {
    (for
      c <- client.attemptT
      forecast: Forecast <- EitherT(
        c.fetchForecast(
          Latitude(39.7456),
          Longitude(-97.0892)
        )
      )

      // since this is an integration test, we can't rely on any of the data coming back so these assertions are weak.
      _ = forecast.shortForecast.nonEmpty.mustBe(true) // ugly and unhelpful.
    yield ()).value.unsafeRunSync() match
      case Right(forecast) => ()
      case Left(e: Throwable) => throw e
  }

  test("IT test: pull forecast from NWS with bad lat/lon errors as expected") {
    (for
      c <- client.attemptT
      forecast: Forecast <- EitherT(
        c.fetchForecast(
          Latitude(100.0), // this lat/lon is not found at NWS. It will 404
          Longitude(100.0)
        )
      )

      // since this is an integration test, we can't rely on any of the data coming back so these assertions are weak.
      _ = forecast.shortForecast.nonEmpty.mustBe(true) // ugly and unhelpful.
    yield ()).value.unsafeRunSync() match
      case Right(forecast) => fail("expecting failure, but got a response from NWS")

      // brittle assertion as NWS might morph. Such is the life of IT tests.
      case Left(e) => e.getMessage.contains("404 Not Found for request GET").mustBe(true)
  }

/**
 * arguably, this data would be better as files in /resources
 */
object TestData:
  val pointJson =
    """
      {
      "@context": [
          "https://geojson.org/geojson-ld/geojson-context.jsonld",
          {
              "@version": "1.1",
              "wx": "https://api.weather.gov/ontology#",
              "s": "https://schema.org/",
              "geo": "http://www.opengis.net/ont/geosparql#",
              "unit": "http://codes.wmo.int/common/unit/",
              "@vocab": "https://api.weather.gov/ontology#",
              "geometry": {
                  "@id": "s:GeoCoordinates",
                  "@type": "geo:wktLiteral"
              },
              "city": "s:addressLocality",
              "state": "s:addressRegion",
              "distance": {
                  "@id": "s:Distance",
                  "@type": "s:QuantitativeValue"
              },
              "bearing": {
                  "@type": "s:QuantitativeValue"
              },
              "value": {
                  "@id": "s:value"
              },
              "unitCode": {
                  "@id": "s:unitCode",
                  "@type": "@id"
              },
              "forecastOffice": {
                  "@type": "@id"
              },
              "forecastGridData": {
                  "@type": "@id"
              },
              "publicZone": {
                  "@type": "@id"
              },
              "county": {
                  "@type": "@id"
              }
          }
      ],
      "id": "https://api.weather.gov/points/39.7456,-97.0892",
      "type": "Feature",
      "geometry": {
          "type": "Point",
          "coordinates": [
              -97.0892,
              39.7456
          ]
      },
      "properties": {
          "@id": "https://api.weather.gov/points/39.7456,-97.0892",
          "@type": "wx:Point",
          "cwa": "TOP",
          "forecastOffice": "https://api.weather.gov/offices/TOP",
          "gridId": "TOP",
          "gridX": 32,
          "gridY": 81,
          "forecast": "https://api.weather.gov/gridpoints/TOP/32,81/forecast",
          "forecastHourly": "https://api.weather.gov/gridpoints/TOP/32,81/forecast/hourly",
          "forecastGridData": "https://api.weather.gov/gridpoints/TOP/32,81",
          "observationStations": "https://api.weather.gov/gridpoints/TOP/32,81/stations",
          "relativeLocation": {
              "type": "Feature",
              "geometry": {
                  "type": "Point",
                  "coordinates": [
                      -97.086661,
                      39.679376
                  ]
              },
              "properties": {
                  "city": "Linn",
                  "state": "KS",
                  "distance": {
                      "unitCode": "wmoUnit:m",
                      "value": 7366.9851976444
                  },
                  "bearing": {
                      "unitCode": "wmoUnit:degree_(angle)",
                      "value": 358
                  }
              }
          },
          "forecastZone": "https://api.weather.gov/zones/forecast/KSZ009",
          "county": "https://api.weather.gov/zones/county/KSC201",
          "fireWeatherZone": "https://api.weather.gov/zones/fire/KSZ009",
          "timeZone": "America/Chicago",
          "radarStation": "KTWX"
      }
  }
      """

  val forecastJson =
    """
      {
      "@context": [
          "https://geojson.org/geojson-ld/geojson-context.jsonld",
          {
              "@version": "1.1",
              "wx": "https://api.weather.gov/ontology#",
              "geo": "http://www.opengis.net/ont/geosparql#",
              "unit": "http://codes.wmo.int/common/unit/",
              "@vocab": "https://api.weather.gov/ontology#"
          }
      ],
      "type": "Feature",
      "geometry": {
          "type": "Polygon",
          "coordinates": [
              [
                  [
                      -97.0799,
                      39.7451
                  ],
                  [
                      -97.0803,
                      39.7672
                  ],
                  [
                      -97.109,
                      39.7668
                  ],
                  [
                      -97.10849999999999,
                      39.744800000000005
                  ],
                  [
                      -97.0799,
                      39.7451
                  ]
              ]
          ]
      },
      "properties": {
          "units": "us",
          "forecastGenerator": "BaselineForecastGenerator",
          "generatedAt": "2025-09-24T17:03:42+00:00",
          "updateTime": "2025-09-24T14:13:26+00:00",
          "validTimes": "2025-09-24T08:00:00+00:00/P7DT17H",
          "elevation": {
              "unitCode": "wmoUnit:m",
              "value": 441.96
          },
          "periods": [
              {
                  "number": 1,
                  "name": "This Afternoon",
                  "startTime": "2025-09-24T12:00:00-05:00",
                  "endTime": "2025-09-24T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 74,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 1
                  },
                  "windSpeed": "10 mph",
                  "windDirection": "N",
                  "icon": "https://api.weather.gov/icons/land/day/sct?size=medium",
                  "shortForecast": "Mostly Sunny",
                  "detailedForecast": "Mostly sunny, with a high near 74. North wind around 10 mph."
              },
              {
                  "number": 2,
                  "name": "Tonight",
                  "startTime": "2025-09-24T18:00:00-05:00",
                  "endTime": "2025-09-25T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 55,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "0 to 5 mph",
                  "windDirection": "NW",
                  "icon": "https://api.weather.gov/icons/land/night/few?size=medium",
                  "shortForecast": "Mostly Clear",
                  "detailedForecast": "Mostly clear, with a low around 55. Northwest wind 0 to 5 mph."
              },
              {
                  "number": 3,
                  "name": "Thursday",
                  "startTime": "2025-09-25T06:00:00-05:00",
                  "endTime": "2025-09-25T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 78,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "0 to 5 mph",
                  "windDirection": "W",
                  "icon": "https://api.weather.gov/icons/land/day/few?size=medium",
                  "shortForecast": "Sunny",
                  "detailedForecast": "Sunny, with a high near 78. West wind 0 to 5 mph."
              },
              {
                  "number": 4,
                  "name": "Thursday Night",
                  "startTime": "2025-09-25T18:00:00-05:00",
                  "endTime": "2025-09-26T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 54,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "0 mph",
                  "windDirection": "",
                  "icon": "https://api.weather.gov/icons/land/night/skc?size=medium",
                  "shortForecast": "Clear",
                  "detailedForecast": "Clear, with a low around 54. South wind around 0 mph."
              },
              {
                  "number": 5,
                  "name": "Friday",
                  "startTime": "2025-09-26T06:00:00-05:00",
                  "endTime": "2025-09-26T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 80,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "5 mph",
                  "windDirection": "SW",
                  "icon": "https://api.weather.gov/icons/land/day/skc?size=medium",
                  "shortForecast": "Sunny",
                  "detailedForecast": "Sunny, with a high near 80. Southwest wind around 5 mph."
              },
              {
                  "number": 6,
                  "name": "Friday Night",
                  "startTime": "2025-09-26T18:00:00-05:00",
                  "endTime": "2025-09-27T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 56,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "5 mph",
                  "windDirection": "S",
                  "icon": "https://api.weather.gov/icons/land/night/few?size=medium",
                  "shortForecast": "Mostly Clear",
                  "detailedForecast": "Mostly clear, with a low around 56. South wind around 5 mph."
              },
              {
                  "number": 7,
                  "name": "Saturday",
                  "startTime": "2025-09-27T06:00:00-05:00",
                  "endTime": "2025-09-27T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 80,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "5 mph",
                  "windDirection": "NW",
                  "icon": "https://api.weather.gov/icons/land/day/few?size=medium",
                  "shortForecast": "Sunny",
                  "detailedForecast": "Sunny, with a high near 80. Northwest wind around 5 mph."
              },
              {
                  "number": 8,
                  "name": "Saturday Night",
                  "startTime": "2025-09-27T18:00:00-05:00",
                  "endTime": "2025-09-28T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 56,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 0
                  },
                  "windSpeed": "5 mph",
                  "windDirection": "E",
                  "icon": "https://api.weather.gov/icons/land/night/skc?size=medium",
                  "shortForecast": "Clear",
                  "detailedForecast": "Clear, with a low around 56. East wind around 5 mph."
              },
              {
                  "number": 9,
                  "name": "Sunday",
                  "startTime": "2025-09-28T06:00:00-05:00",
                  "endTime": "2025-09-28T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 82,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 1
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "SE",
                  "icon": "https://api.weather.gov/icons/land/day/few?size=medium",
                  "shortForecast": "Sunny",
                  "detailedForecast": "Sunny, with a high near 82. Southeast wind 5 to 10 mph."
              },
              {
                  "number": 10,
                  "name": "Sunday Night",
                  "startTime": "2025-09-28T18:00:00-05:00",
                  "endTime": "2025-09-29T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 60,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 1
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "S",
                  "icon": "https://api.weather.gov/icons/land/night/few?size=medium",
                  "shortForecast": "Mostly Clear",
                  "detailedForecast": "Mostly clear, with a low around 60."
              },
              {
                  "number": 11,
                  "name": "Monday",
                  "startTime": "2025-09-29T06:00:00-05:00",
                  "endTime": "2025-09-29T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 82,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 2
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "S",
                  "icon": "https://api.weather.gov/icons/land/day/sct?size=medium",
                  "shortForecast": "Mostly Sunny",
                  "detailedForecast": "Mostly sunny, with a high near 82."
              },
              {
                  "number": 12,
                  "name": "Monday Night",
                  "startTime": "2025-09-29T18:00:00-05:00",
                  "endTime": "2025-09-30T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 61,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 4
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "S",
                  "icon": "https://api.weather.gov/icons/land/night/sct?size=medium",
                  "shortForecast": "Partly Cloudy",
                  "detailedForecast": "Partly cloudy, with a low around 61."
              },
              {
                  "number": 13,
                  "name": "Tuesday",
                  "startTime": "2025-09-30T06:00:00-05:00",
                  "endTime": "2025-09-30T18:00:00-05:00",
                  "isDaytime": true,
                  "temperature": 81,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 4
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "S",
                  "icon": "https://api.weather.gov/icons/land/day/sct?size=medium",
                  "shortForecast": "Mostly Sunny",
                  "detailedForecast": "Mostly sunny, with a high near 81."
              },
              {
                  "number": 14,
                  "name": "Tuesday Night",
                  "startTime": "2025-09-30T18:00:00-05:00",
                  "endTime": "2025-10-01T06:00:00-05:00",
                  "isDaytime": false,
                  "temperature": 60,
                  "temperatureUnit": "F",
                  "temperatureTrend": "",
                  "probabilityOfPrecipitation": {
                      "unitCode": "wmoUnit:percent",
                      "value": 5
                  },
                  "windSpeed": "5 to 10 mph",
                  "windDirection": "SE",
                  "icon": "https://api.weather.gov/icons/land/night/few?size=medium",
                  "shortForecast": "Mostly Clear",
                  "detailedForecast": "Mostly clear, with a low around 60."
              }
          ]
      }
  }
      """