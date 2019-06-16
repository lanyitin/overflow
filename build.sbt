lazy val huevo = RootProject(file("../huevo"))
lazy val commonGraph = RootProject(file("../common-graph"))

lazy val root = project
  .in(file("."))
  .aggregate(huevo, commonGraph)
  .dependsOn(commonGraph, huevo)
  .settings(
    name := "overflow",
    version := "0.1.0",

    // To make the default compiler and REPL use Dotty
    scalaVersion := "2.12.8",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.25",

    libraryDependencies += "dom4j" % "dom4j" % "1.6.1",
    libraryDependencies += "jaxen" % "jaxen" % "1.2.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.9",
    libraryDependencies += "commons-cli" % "commons-cli" % "1.4",

  )
