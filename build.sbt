// BUILD THE CURRENT PACKAGE JAR BY:
//    sbt clean package
// BUILD AND PUBLISH THE ARTEFACT LOCALLY BY:
//    sbt clean publishLocal
// OR BUILD AS AN UBER-JAR BY:
//    sbt clean assembly
// THE UBER-JAR includes (unpacked) all the dependencies needed to run
// as a java program. (>40mb)
//
// java -jar glyph+skija.jar # runs org.sufrin/glyph.tests.GlyphBook.GlyphBook
// java -cp glyph+skija.jar org.sufrin.glyph.tests.ANEXAMPLE # runs ANEXAMPLE
//
// The file: lib/Logging.jar (also in SkijaLib) is the only unmanaged
// component needed to use the glyph library.
//

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / fork := true

ThisBuild / crossPaths := false
ThisBuild / organization := "org.sufrin"
ThisBuild / name := "glyph"
ThisBuild / version := "0.9.0"

// This is essential to avoid problems with pre java9 Cleanable class not being found
// Skija itself is multi-release (see the shared skija jar manifest)
Compile / packageOptions += Package.ManifestAttributes("Multi-Release" -> "true")

// Fat jar assembly 
enablePlugins(AssemblyPlugin)

assembly / assemblyJarName := "glyph+skija.jar"
assembly / mainClass := Some("org.sufrin.glyph.tests.GlyphBook.GlyphBook")
assembly / assemblyMergeStrategy := {
  case PathList("io", "github", "humbleui", "skija", "impl", "Cleanable.class") =>
        MergeStrategy.discard       // the pre version 9 variant
  case PathList("META-INF", "versions", "9", xs @ _*) =>
        MergeStrategy.first         // include the version 9+ variant(s)
  case PathList("META-INF", xs @ _*) =>
        MergeStrategy.discard       // remove unnecessary META-INF files
  case _ =>
        MergeStrategy.first
}


scalacOptions ++= Seq(
    "-encoding",   "UTF-8",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-Xfatal-warnings"
  )
  
lazy val root = (project in file("."))
  .aggregate(Utilities)
  .dependsOn(Utilities)
  .settings(
    name := "Glyph",
    idePackagePrefix := Some("org.sufrin.glyph"),

  )

lazy val Utilities = (project in file("Utilities"))
  .settings(
    name := "Utilities",
    idePackagePrefix := Some("org.sufrin.utility"),
  )

// Documentation merge
lazy val mergeApi = taskKey[Unit]("Merge Scaladoc API directories")

mergeApi := {
  import scala.sys.process._
  val base = (ThisBuild / baseDirectory).value
  val rootApi = base / "target" / "api"
  val UtilitiesApi = base / "Utilities" / "target" / "api"

  Seq("bash", "-c", s"mv $UtilitiesApi/ $rootApi/UtilitiesApi").!
  streams.value.log.info(s"Moved $UtilitiesApi to $rootApi/UtilitiesApi")
}


// XML GlyphML for paragraphs

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.2.0"


//publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/.m2/repository")))


resolvers += Resolver.file("local-ivy", new File(Path.userHome.absolutePath + "/.ivy2/repository"))(Resolver.ivyStylePatterns)


val skijaVersion = "0.116.2"
val jwmVersion = "0.4.17"
libraryDependencies ++= Seq(
  "io.github.humbleui" % "skija-linux-x64" % skijaVersion,
  "io.github.humbleui" % "skija-macos-x64" % skijaVersion,
  "io.github.humbleui" % "skija-macos-arm64" % skijaVersion,
  "io.github.humbleui" % "skija-shared" % skijaVersion,
  "io.github.humbleui" % "jwm" % jwmVersion,
)

