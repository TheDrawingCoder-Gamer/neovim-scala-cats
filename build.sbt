val scala3Version = "3.3.1"
val circeVersion = "0.14.1"
lazy val root = project
  .in(file("."))
  .settings(
    name := "neovim-bindings",
    version := "0.4.0-SNAPSHOT",
    organization := "io.github.TheDrawingCoder-Gamer",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.2", 
    libraryDependencies += "org.msgpack" % "msgpack-core" % "0.9.3",
    libraryDependencies += "io.reactivex.rxjava3" % "rxjava" % "3.1.5",
    libraryDependencies += "org.typelevel" %% "munit-cats-effect" % "2.0.0-M4" % Test,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.9.3",
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.9.3"

  )
