lazy val root = project
  .in(file("."))
  .settings(
    name := "mist2",
    version := "0.1.0",

    scalaVersion := "0.24.0-RC1",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
