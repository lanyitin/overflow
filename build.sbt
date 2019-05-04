val dottyVersion = "0.14.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "overflow",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.25"
  )
