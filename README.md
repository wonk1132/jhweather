For a localhost setup for scala development, there is likely nothing to do except to clone this project in order
to run it either with SBT directly or with an editor such as IntelliJ.

# Hints:
 - this uses SBT, I assume you have that set up
 - to start up the server, you can `sbt run` from the root of this directory. That should start the server up.
 - with the host up, you can `curl http://localhost:8080/forecast/short/lat/39.3/lon/-97.08` to get a forecast. (and valid US lat/lon shouold give back a value)
 - not all lat/lon are supported (the National Weather Service itself does not support all lat/lon), so if we pass them a lat/lon they do not support they could 404.
 - you can run the test suite with a `sbt test`, or with an IDE. I use IntelliJ but VS code should work no problem.
