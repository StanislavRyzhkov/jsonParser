name := "customParser"

version := "0.1"

scalaVersion := "2.13.6"

lazy val common = (project in file("."))
  .enablePlugins(ScalafmtPlugin)
  .enablePlugins(AssemblyPlugin)
  .settings(
    name := "customParser",
    libraryDependencies ++= Seq(),
    scalacOptions ++= Seq(
      "-feature",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:postfixOps"
    )
  )
