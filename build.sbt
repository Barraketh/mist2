val common = List(
  scalaVersion := "3.0.0-M2",
  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.11" % "test"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
)

lazy val pegDotty = project
  .in(file("peg-dotty"))
  .settings(
    name := "peg-dotty",
    version := "0.1.0",
  ).settings(common)

lazy val lang = project
  .in(file("lang"))
  .settings(
    name := "lang",
    version := "0.1.0",
  ).settings(common)
  .dependsOn(pegDotty)