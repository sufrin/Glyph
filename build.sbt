ThisBuild / version := "0.9.0"

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
    name := "Glyph",
    idePackagePrefix := Some("org.sufrin.glyph"),

  )

// XML GlyphML for paragraphs

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.2.0"
