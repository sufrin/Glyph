// BUILD AND PUBLISH THE ARTEFACT BY: rm -rf ~/.ivy2/local/org.sufrin/glyph; sbt clean update publishLocal

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / fork := true



ThisBuild / crossPaths := false
ThisBuild / organization := "org.sufrin"
ThisBuild / name := "glyph"
ThisBuild / version := "0.9.0"
ThisBuild / artifactName := {
  (sv: ScalaVersion, mod: ModuleID, artifact: Artifact) =>
  "glyph-" + mod.revision + "." + artifact.extension
}


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


//publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/.m2/repository")))


resolvers += Resolver.file("local-ivy", new File(Path.userHome.absolutePath + "/.ivy2/repository"))(Resolver.ivyStylePatterns)

