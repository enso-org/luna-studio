name := "ProjectManager"
organization := "org.luna-lang"
version := "0.1"

scalaVersion := "2.12.8"

val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.5.23"
val akkaStream = "com.typesafe.akka" %% "akka-stream" % "2.5.23"
val akkaHttp = "com.typesafe.akka" %% "akka-http" % "10.1.8"
val akkaSpray = "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.8"
val circe = Seq("circe-core", "circe-generic", "circe-yaml").map(
  "io.circe" %% _ % "0.10.0"
)
val main = Some("luna.projectmanager.Server")
//val pkgProject = ProjectRef(uri("git://github.com/luna/enso#packages"), "pkg")
val pkgProject = ProjectRef(file("/Users/marcinkostrzewa/code/enso"), "pkg")
lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      akkaActor,
      akkaStream,
      akkaHttp,
      akkaSpray
    )
  )
  .settings(libraryDependencies ++= circe)
  .settings(libraryDependencies += "io.spray" %% "spray-json" % "1.3.5")
  .settings(Compile / mainClass := main)
  .settings(assembly / mainClass := main)
  .settings(
    assembly / assemblyOutputPath := new File(
      "../../../dist/bin/private/luna-project-manager.jar"
    )
  )
  .dependsOn(pkgProject)
