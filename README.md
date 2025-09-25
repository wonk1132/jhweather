# Running this Service

With localhost setup for scala development, there is likely nothing to do except to clone this project in order
to run it either with SBT directly or with an editor such as IntelliJ.

# Hints:
 - this uses SBT, I assume you have that set up
- to start up the server, you can `sbt run` from the root of this directory. That should start the server up, assuming you don't already have port 8080 in use. You can change the port in that case in the configuration file found ih the root package object:
  ```scala
  // package.scala  
  trait WeatherServiceConfig:
    val runtimeEnvironment: RuntimeEnvironment
    val nationalWeatherServiceUri: String
    val port: Port = port"8080" // change the port to one you aren't using here
  ```
 - with the host up, you can `curl http://localhost:8080/forecast/short/lat/39.3/lon/-97.08` to get a forecast. 
 - not all lat/lon are supported (the National Weather Service itself does not support all lat/lon), so if we pass them a lat/lon they do not support they could 404.
 - you can run the test suite with a `sbt test`, or with an IDE. I use IntelliJ but VS code should work no problem.
