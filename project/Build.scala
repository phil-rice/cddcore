import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys

object CddBuild extends Build {

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
   
    version := "2.0.1",
    scalacOptions ++= Seq(),
    retrieveManaged := false,
    scalaVersion := "2.10.4",
    EclipseKeys.withSource := true,
    resolvers += Classpaths.typesafeResolver,
	resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
	
	
  lazy val build = Project(id = "build", settings = buildSettings, base = file("build"))
  lazy val cddcore = Project(id = "cddcore", settings = buildSettings, base = file("cddcore")).dependsOn(engine, htmlRendering,cddjunit, structure)
  lazy val engine = Project(id = "engine", settings = buildSettings, base = file("engine"))
  lazy val structure =Project(id = "structure", settings = buildSettings, base = file("structure")).dependsOn(engine)
  lazy val htmlRendering = Project(id = "htmlRendering", settings = buildSettings, base = file("htmlRendering")).dependsOn(engine)
  lazy val cddjunit = Project(id = "cddjunit", settings = buildSettings, base = file("cddjunit")).dependsOn( engine,htmlRendering)
  lazy val website = Project(id = "website",settings = buildSettings,  base = file("website")).dependsOn( engine,htmlRendering)
  lazy val examples = Project(id = "examples",settings = buildSettings,  base = file("examples")).dependsOn(engine,cddjunit, structure,website)
  lazy val tests = Project(id = "tests", settings = buildSettings, base = file("tests")).dependsOn( engine, htmlRendering,cddjunit, structure,website, examples)
//  lazy val root = Project(id = "root",settings = buildSettings,  base = file(".")).aggregate( engine, tests,cddjunit)
  lazy val root = Project(id = "root", settings = buildSettings, base = file(".")).aggregate( engine, tests, htmlRendering, structure, cddjunit,website, examples, build)
}
