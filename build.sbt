name := "as-blog-examples"

version := "1.0"

scalaVersion := "2.12.6"

lazy val akkaVersion = "2.5.25"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion
)
