lazy val root = project
  .in(file("."))
  .settings(
    name := "overflow",
    version := "0.1.0",
    // required by cats
    scalacOptions += "-Ypartial-unification",
    // To make the default compiler and REPL use Dotty
    scalaVersion := "2.12.8",
    // Test Related Libraries
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    // Core Libraries
    libraryDependencies += "tw.lanyitin.common" % "common-graph" % "0.1.0",
    libraryDependencies += "tw.lanyitin" % "huevo" % "0.2.0",

    // Useful Common Libraries
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.9",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.7",
    libraryDependencies += "commons-cli" % "commons-cli" % "1.4",

    // Logging Facade
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.25",

    // YAML Parsing
    libraryDependencies += "io.circe" %% "circe-yaml" % "0.9.0",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.9.0",

    libraryDependencies += "dom4j" % "dom4j" % "1.6.1",
    libraryDependencies += "jaxen" % "jaxen" % "1.2.0",

    libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1"
  )
