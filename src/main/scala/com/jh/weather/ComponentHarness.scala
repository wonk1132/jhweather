package com.jh.weather

import cats.data.Kleisli
import cats.effect.Async
import cats.syntax.all.*

/** The Component Harness is an abstraction across all of the components used by the system. This
  * allows us to spin up all of the components for a given environment (Dev, Test, Prod, etc) as
  * the service spins up, housing all of these environment-specific components in a single place.
  */
trait ComponentHarness[F[_]]:
  val configuration: WeatherServiceConfig
  val metrics: Metrics[F]
  val logger: Logger[F]
  val routes: Kleisli[F, org.http4s.Request[F], org.http4s.Response[F]]
  val nationalWeatherService: F[NationalWeatherService[F]]

object ComponentHarness:
  def fetchComponentHarnessForEnvironment[F[_]: Async](
    config: WeatherServiceConfig
  ): Either[Throwable, ComponentHarness[F]] =
    config.runtimeEnvironment match
      case RuntimeEnvironment.Localhost =>
        for metricInstance <- Metrics.apply[F](config)
        yield new ComponentHarness[F]:
          val configuration: WeatherServiceConfig = config
          val logger: Logger[F] = Logger.apply[F]
          val metrics = metricInstance

          given Logger[F] = logger
          given Metrics[F] = metrics

          val nationalWeatherService: F[NationalWeatherService[F]] =
            NationalWeatherService.apply[F](config)
          val routes = WeatherRoutes.apply(nationalWeatherService)
      case x => Throwable(s"no ComponentHarness defined for env '$x'").asLeft
