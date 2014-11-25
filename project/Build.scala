import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

object CddBuild extends Build {

  val cddVersionNo = "2.1.2"
//  val scalaVersionNo = "2.11.3"
  val scalaVersionNo = "2.10.4"
  val scalaTestVersionNo = "2.2.0"
    
  val buildSettings = Defaults.defaultSettings ++  Seq(
   (testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "report"),
    publishMavenStyle := true,
    parallelExecution in Global := false,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },

    publishArtifact in Test := false,

    pomIncludeRepository := { _ => false },

    pomExtra := (
      <url>http://www.constraintdrivendevelopment.com</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>https://github.com/phil-rice/autoTdd</url>
        <connection>git@github.com:phil-rice/cddcore.git</connection>
      </scm>
      <developers>
        <developer>
          <id>phil.rice</id>
          <name>Phil Rice</name>
          <url>http://www.constraintdrivendevelopment.org</url>
        </developer>
      </developers>),
    unmanagedClasspath in Runtime <+= (baseDirectory) map { bd => Attributed.blank(bd / "src/main/resources") },
    organization := "org.cddcore",
   
    version := cddVersionNo,
    scalacOptions ++= Seq(),
    retrieveManaged := false,
    //scalaVersion := "2.10.4",  //need to change this in engine project as well
    scalaVersion := scalaVersionNo,
    EclipseKeys.withSource := true,
    resolvers += Classpaths.typesafeResolver,
	resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
	libraryDependencies := {
       CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.1")
    case _ =>
      // or just libraryDependencies.value if you don't depend on scala-swing
      libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
  }
}
	
	)
	
	
  lazy val build = Project(id = "build", settings = buildSettings, base = file("build"))
  lazy val cddcore = Project(id = "cddcore", settings = buildSettings, base = file("cddcore")).dependsOn(engine, htmlRendering,cddjunit, structure)
  lazy val engine = Project(id = "engine", settings = buildSettings, base = file("engine"))
  lazy val structure =Project(id = "structure", settings = buildSettings, base = file("structure")).dependsOn(engine)
  lazy val htmlRendering = Project(id = "htmlRendering", settings = buildSettings, base = file("htmlRendering")).dependsOn(engine)
  lazy val cddjunit = Project(id = "cddjunit", settings = buildSettings, base = file("cddjunit")).dependsOn( engine,htmlRendering)
  lazy val website = Project(id = "website",settings = buildSettings,  base = file("website")).dependsOn( engine,htmlRendering)
  lazy val examples = Project(id = "examples",settings = buildSettings,  base = file("examples")).dependsOn(engine,cddjunit, structure,website, legacy)
  lazy val legacy = Project(id = "legacy",settings = buildSettings,  base = file("legacy")).dependsOn(engine,cddjunit, structure,website)
  lazy val tests = Project(id = "tests", settings = buildSettings, base = file("tests")).dependsOn( engine, htmlRendering,cddjunit, structure,website, examples, legacy)
//  lazy val root = Project(id = "root",settings = buildSettings,  base = file(".")).aggregate( engine, tests,cddjunit)
  lazy val root = Project(id = "root", settings = buildSettings, base = file(".")).aggregate( engine, tests, htmlRendering, structure, cddjunit,website, examples, build,legacy)
}
