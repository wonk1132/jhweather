import scala.collection.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

val catsVer = "3.6.3"
val logForCatsVer = "2.7.1"
val http4sVersion = "1.0.0-M38"
val circeVersion = "0.14.14"

val scalacOpts: Seq[String] = Seq(
  "-Wvalue-discard",
  "-Xkind-projector",
  "-Xfatal-warnings",
  "-Wunused:all",
  "-deprecation",
  "-feature",
  "-explain-types",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
)

lazy val weather = (project in file("."))
  .settings(
    name := "weather",
    libraryDependencies := Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % catsVer,
      "org.typelevel" %% "cats-effect-std" % catsVer,
      "org.scalatest" %% "scalatest" % "3.2.19"  % "test",
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s"  %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
    )

  )

