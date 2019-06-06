name := "ProjectManager"
organization := "org.luna-lang"
version := "0.1"

scalaVersion := "2.12.8"

val akkaActor  = "com.typesafe.akka" %% "akka-actor"  % "2.5.23"
val akkaStream = "com.typesafe.akka" %% "akka-stream" % "2.5.23"
val akkaHttp   = "com.typesafe.akka" %% "akka-http"   % "10.1.8"
val akkaSpray  = "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.8"
val circe = Seq("circe-core", "circe-generic", "circe-yaml").map("io.circe" %% _ % "0.10.0")

libraryDependencies ++= Seq(akkaActor, akkaStream, akkaHttp, akkaSpray)
libraryDependencies ++= circe
libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"