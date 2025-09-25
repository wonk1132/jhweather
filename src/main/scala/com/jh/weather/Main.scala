package com.jh.weather

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import com.jh.weather.*
import org.http4s.ember.server.*
import org.http4s.server.Server

object Main extends IOApp:

  /** If this was written "for real" it would have the goo to figure out where it was spun up
    * (local/dev/prod/etc) so it could return the correct RuntimeEnvironment for where the service
    * is currently running. In this assignment, I'm just going to return Localhost.
    */
  private def fetchRuntimeEnvironment: Either[Throwable, RuntimeEnvironment] =
    RuntimeEnvironment.Localhost.asRight

  /** Entrypoint. Starts HTTP4s server, listens for inbound requests
    */
  def run(args: List[String]): IO[ExitCode] =
    (for
      // decipher where we are running (local/dev/prod/etc)
      runtimeEnvironment <- EitherT.fromEither[IO](fetchRuntimeEnvironment)

      // get the config info for our current runtime environment
      config <- EitherT.fromEither[IO](
        WeatherServiceConfig.fetchForRuntimeEnvironment(runtimeEnvironment)
      )

      // get the components configured for this runtime environment
      componentHarness <- EitherT.fromEither[IO](
        ComponentHarness.fetchComponentHarnessForEnvironment[IO](config)
      )
    yield componentHarness).value
      .flatMap:
        case Left(error) =>
          // start up failure mode--would need to know more about target env to write something that panics loudly,
          // or perhaps not if there was infra tooling that would handle that.
          val logger = Logger.apply[IO]
          for _ <- logger.error(s"Failed to start: ${error.getMessage}")
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
              _ <- Resource.eval(
                componentHarness.logger.info("server started! Awaiting inbound requests...")
              )
            yield server

          server
            .use { _ => IO.never }
            .as(ExitCode.Success)
