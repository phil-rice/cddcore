import sbt._
import Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys



object BuildSettings {

  val buildSettings = Defaults.defaultSettings ++ Seq(
    publishMavenStyle := true,

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
      <url>http://www.autotdd.com</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>https://github.com/phil-rice/autoTdd</url>
        <connection>https://github.com/phil-rice/autoTdd.git</connection>
      </scm>
      <developers>
        <developer>
          <id>phil.rice</id>
          <name>Phil Rice</name>
          <url>http://www.autotdd.org</url>
        </developer>
      </developers>),

    organization := "org.autotdd",
    //version := "1.2.0-SNAPSHOT",
    version := "1.2.0",
    scalacOptions ++= Seq(),
    retrieveManaged := false,
    scalaVersion := "2.10.1",
    EclipseKeys.withSource := true,
    EclipseKeys.eclipseOutput := Some("bin"),
    resolvers += Classpaths.typesafeResolver,
// 
    libraryDependencies ++= Seq(
	  "org.scalaj" % "scalaj-time_2.10.2" % "0.7",
	  "org.hsqldb" % "hsqldb" % "2.0.0",     
      "mysql" % "mysql-connector-java" % "5.1.6",            	  
	  "commons-dbcp" % "commons-dbcp" % "1.2.2",            
      "org.springframework" % "spring-jdbc" % "3.2.3.RELEASE",   
      "org.scala-stm" %% "scala-stm" % "0.7",
      "org.scala-lang" % "scala-reflect" % "2.10.1",
      "org.scala-lang" % "scala-compiler" % "2.10.1",
      "org.scalatest" % "scalatest_2.10" % "1.9.1",
      "log4j" % "log4j" % "1.2.17",
      "junit" % "junit" % "4.8.2"))


	  
  val eclipseSettings = buildSettings ++ Seq(
    resolvers += "eclipse-repo" at "https://swt-repo.googlecode.com/svn/repo/",
    libraryDependencies ++= Seq(
      "com.miglayout" % "miglayout-swt" % "4.2",
      "org.autotdd" %% "constraint" % "1.0.0-SNAPSHOT",
      "org.autotdd" %% "engine" % "1.0.0-SNAPSHOT",
      "org.eclipse.equinox" % "org.eclipse.equinox.common" % "3.6.0.v20100503",
      "org.eclipse.ui" % "org.eclipse.ui.workbench" % "3.7.1.v20120104-1859",
      "org.eclipse.swt.win32.win32" % "x86" % "3.3.0-v3346",
      "org.eclipse.core" % "org.eclipse.core.runtime" % "3.6.0.v20100505"))
	  
	  
  val javanetDeps = "javanetDeps" at "http://download.java.net/maven/2/"
 
 val exampleSettings = buildSettings ++ Seq(
    //resolvers += "eclipse-repo" at "https://swt-repo.googlecode.com/svn/repo/",
    libraryDependencies ++= Seq(
       "com.sun.jersey" % "jersey-server" % "1.2",
      "com.sun.jersey" % "jersey-json" % "1.2",
      "org.eclipse.jetty" % "jetty-server" % "8.0.0.M0",
     "org.eclipse.jetty" % "jetty-servlet" % "8.0.0.M0"))

}

object HelloBuild extends Build {
  import BuildSettings._

  val copy = TaskKey[Unit]("copy", "Copies files to eclipse project")

  val copyTask = copy := {
    copy("lib_managed", "eclipse2/libFromSbt")
  }

  lazy val constraint = Project(id = "constraint", settings = buildSettings, base = file("constraint"))
  lazy val engine = Project(id = "engine", settings = buildSettings, base = file("engine")) dependsOn (constraint)
  lazy val engine_test = Project(id = "engine_test", settings = buildSettings, base = file("engine-tests")) dependsOn (constraint, engine)
  lazy val examples = Project(id = "examples", settings = exampleSettings, base = file("examples")) dependsOn (constraint, engine)  
  lazy val carers = Project(id = "carers", settings = buildSettings, base = file("carers")) dependsOn (constraint, engine)
  lazy val timing = Project(id = "timing", settings = buildSettings, base = file("timing")) dependsOn (constraint, engine, carers)
  lazy val eclipse = Project(id = "eclipse", settings = eclipseSettings ++ Seq(copyDepTask), base = file("eclipse2")) // dependsOn (constraint, engine)
  lazy val root = Project(id = "root", settings = buildSettings ++ Seq(copyTask, copyDepTask), base = file(".")) aggregate (constraint, engine, examples, engine_test,carers, timing)

  import java.io.File

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    if (these == null)
      Array();
    else
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def copy(start: String, to: String) {
    val s = new File(start)
    val t = new File(to)
    for (
      f <- recursiveListFiles(s) //
      if f.getName().endsWith(".jar") //
      if (!f.getPath().contains("org.eclipse")) //
      if (!f.getPath().contains("org.osgi")) //
      if (!f.getPath().contains("servlet-api"))
    ) {
      val d = new File(t, f.getName())
      println("Copying " + f + " to " + d);
      copy(f, d)
    }
  }

  def copy(sourceFile: File, destFile: File) {
    import java.nio.channels.FileChannel
    import java.io.FileInputStream
    import java.io.FileOutputStream

    if (!destFile.exists())
      destFile.createNewFile();

    val source = new FileInputStream(sourceFile).getChannel();
    val destination = new FileOutputStream(destFile).getChannel();
    destination.transferFrom(source, 0, source.size());
  }
  lazy val copyDependencies = TaskKey[Unit]("pack")

  def copyDepTask = copyDependencies <<= (update, crossTarget, scalaVersion) map {
    (updateReport, out, scalaVer) =>
      updateReport.allFiles foreach {
        srcPath =>
          val destPath = new File("eclipse2") / "lib" / srcPath.getName
          println(destPath)
          IO.copyFile(srcPath, destPath, preserveLastModified = true)
      }

  }
}
