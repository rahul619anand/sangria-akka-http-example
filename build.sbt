name := "sangria-akka-http-example"
version := "0.1.0-SNAPSHOT"

description := "An example GraphQL server written with akka-http, circe and sangria."

scalaVersion := "2.12.6"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "1.4.1",
  "org.sangria-graphql" %% "sangria-circe" % "1.2.1",
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "de.heikoseeberger" %% "akka-http-circe" % "1.21.0",
  "io.circe" %% "circe-core" % "0.9.3",
  "io.circe" %% "circe-parser" % "0.9.3",
  "io.circe" %% "circe-optics" % "0.9.3",
  "io.circe" %% "circe-generic-extras" % "0.9.3",
  "io.circe" %% "circe-generic" % "0.9.3",
  "com.beachape" %% "enumeratum" % "1.5.12",
  "com.beachape" %% "enumeratum-circe" % "1.5.14",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

Revolver.settings
enablePlugins(JavaAppPackaging)
