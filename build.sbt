val scala3Version = "3.2.1"
val circeVersion = "0.14.1"
lazy val root = project
  .in(file("."))
  .settings(
    name := "neovim-bindings",
    version := "0.2.0-SNAPSHOT",
    organization := "net.bulbyvr",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2", 
    libraryDependencies += "org.msgpack" % "msgpack-core" % "0.9.3",
    libraryDependencies += "io.reactivex.rxjava3" % "rxjava" % "3.1.5",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

  )
