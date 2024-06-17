ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

ThisBuild / fork := true


scalacOptions ++= Seq(
    "-encoding",   "UTF-8",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-Xfatal-warnings"
  )
  
lazy val root = (project in file("."))
  .settings(
    name := "Glyphs",
    idePackagePrefix := Some("org.sufrin.glyph"),

  )
