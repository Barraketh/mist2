lazy val peg = project
  .in(file("peg"))
  .settings(
    name := "peg",
    version := "0.1.0",

    scalaVersion := "2.13.4",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.8" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:missing-interpolator",
      "-Xlint:-inaccessible,-constant,_",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-language:higherKinds",
      "-language:implicitConversions"
    ),

  )
