val dottyVersion = "0.14.0-RC1"
val scala212Version = "2.12.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "overflow",
    version := "0.1.0",

    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.25",

    libraryDependencies += "dom4j" % "dom4j" % "1.6.1",
    libraryDependencies += "jaxen" % "jaxen" % "1.2.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.9",
    libraryDependencies += "commons-cli" % "commons-cli" % "1.4",


    libraryDependencies += "tw.lanyitin" % "huevo_2.12" % "0.1.0",
    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala212Version)
  )
