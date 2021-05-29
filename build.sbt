name := "customParser"

version := "0.1"

scalaVersion := "2.13.6"

lazy val common = (project in file("."))
  .enablePlugins(ScalafmtPlugin)
  .enablePlugins(AssemblyPlugin)
  .settings(
    name := "customParser",
    libraryDependencies ++= Seq(
      "com.chuusai"   %% "shapeless" % "2.3.7",
      "org.scalatest" %% "scalatest" % "3.2.0" % Test
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:postfixOps"
    )
  )
