package org.cddcore.engine.tests

import org.junit.runner.Description
import scala.collection.JavaConversions._
import org.junit.runner.Runner
import org.junit.runner.notification.RunNotifier
import org.cddcore.engine._
import scala.reflect.runtime.{ universe => ru }
import java.lang.reflect.Method
import junit.framework.Assert
import org.junit.runner.notification.Failure
import java.io.File
import scala.collection.mutable.StringBuilder
import sys.process._
import java.lang.reflect.Field
import junit.framework.AssertionFailedError
import org.cddcore.engine.ReportableWithTemplate
import java.util.concurrent.atomic.AtomicBoolean
import org.junit.runner.notification.RunListener
import org.junit.runner.Result
import java.util.concurrent.atomic.AtomicReference

object CddRunner {
  val separator = "\n#########\n"
  val userHome = System.getProperty("user.home");
  var directory = new File(userHome, ".cdd")
  val needToAddListener = new AtomicBoolean(true)
  val enginesInTest = new AtomicReference(List[Engine]())
  def addToEngines(l: List[Engine]) {
    while (true) {
      val original = enginesInTest.get()
      val next = original ::: l
      if (enginesInTest.compareAndSet(original, next))
        return
    }
  }
}
trait NotActuallyFactory[R, FullR] extends EngineUniverse[R, FullR] {
  def builder: RealScenarioBuilder = ???
  def logger: org.cddcore.engine.TddLogger = TddLogger.noLogger
  def rfnMaker: scala.util.Either[() => Exception, Any] => RFn = ???
  def makeClosureForBecause(params: List[Any]) = ???
  def makeClosureForCfg(params: List[Any]) = ???
  def makeClosureForResult(params: List[Any]) = ???
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = ???

}

trait CddRunner extends Runner with EngineUniverse[Any, Any] with NotActuallyFactory[Any, Any] {
  def clazz: Class[_ <: Any]
  def enginesToNameMap: Map[Engine, String]

  def title: String
  lazy val getDescription = {
    val result = Description.createSuiteDescription(title);
    if (Engine.logging) println(enginesToNameMap)
    for ((engine, name) <- enginesToNameMap)
      result.addChild(addEngine(name, engine))
    result
  }

  type BiMap = Tuple2[Map[Reportable, Description], Map[Description, Reportable]]
  import org.cddcore.engine.Engine._
  var exceptionMap: Map[Test, Throwable] = Map()
  var biMap: BiMap = (Map(), Map())
  var allEngines = List[Engine]()
  var names = Set[String]()

  def addEngineForTest(name: String, engine: Any) = addEngine(name, engine.asInstanceOf[Engine])

  /** this code is here for a truely hideous Eclipse interop bug that I wasn't able to duplicate in a test :( Basically if you have two tests with the same name, and one passes and one failes, then the Eclipse JUnit plugin I was using didn't work well */
  def descriptionfor(r: Reportable): Description = {
    var name = descriptionfor(r, "")
    var i = 0
    while (names.contains(name)) {
      i += 1
      name = descriptionfor(r, i.toString)
    }
    name
  }
  def descriptionfor(r: Reportable, postFix: String): Description = {
    val name = Strings.clean(r match {
      case t: Test => t.titleString + " => " + logger(t.expected.getOrElse(""))
      case r: Requirement => r.title.getOrElse("")
      case _ => throw new IllegalStateException(r.getClass() + "/" + r)
    })
    Description.createSuiteDescription(name)
  }

  def addEngine(name: String, engine: Engine): Description = {
    import EngineWithScenarioExceptionMap._
    val engineDescription = Description.createSuiteDescription(name)
    if (Engine.logging) {
      println(name)
      println(engine)
    }
    val result = addChildren(List(engine), engineDescription, (biMap._1 + (engine -> engineDescription), biMap._2 + (engineDescription -> engine)));
    biMap = result
    exceptionMap = exceptionMap ++ engine.scenarioExceptionMap.map
    allEngines = engine :: allEngines

    engineDescription
  }

  def addChildren(path: List[Reportable], d: Description, biMap: BiMap): BiMap = {
    (biMap, path) match {
      case ((from, to), (r: RequirementAndHolder) :: _) =>
        r.children.foldLeft(biMap)((acc, child) => add(child :: path, d, acc))
      case _ => biMap
    }
  }

  def add(path: List[Reportable], parentDescription: Description, biMap: BiMap): BiMap = {
    def makeAndAddDescription(r: Reportable) = {
      val d = descriptionfor(r)
      parentDescription.addChild(d)
      d
    }
    (biMap, path) match {
      case ((from, to), (r: RequirementAndHolder) :: _) =>
        val d = makeAndAddDescription(r)
        r.children.foldLeft((from + (r -> d), to + (d -> r)))((acc, child) => add(child :: path, d, acc))
      case ((from, to), (r: Reportable) :: _) if !from.contains(r) =>
        val d = makeAndAddDescription(r)
        (from + (r -> d), to + (d -> r))
      case (_, x :: _) => biMap //TODO this is awkward We have a duplicate scenario or duplicate 'something'. I need to think about to actually do here. The duplicate scenario exception is currently corrected recorded, so that's something
    }

  }

  def fileFor(clazz: Class[Any], ed: Description, extension: String) = new File(CddRunner.directory, clazz.getName() + "." + ed.getDisplayName() + "." + extension)

  def saveResults(clazz: Class[Any], ed: Description, e: EngineFromTestsImpl) {
    import Files._
    printToFile(fileFor(clazz, ed, "dt.html"))((pw) => pw.write(e.toStringWith(new HtmlIfThenPrinter)))
  }

  def log(s: String) =
    if (Engine.logging)
      println(s)

  def run(notifier: RunNotifier) = {
    val exceptionScenarios = exceptionMap.keySet
    val allScenarios = biMap._1.keySet.collect { case t: Test => t }
    val left = exceptionScenarios -- allScenarios
    if (left.size > 0)
      throw new IllegalStateException
    val description = getDescription
    notifier.fireTestStarted(description)
    for ((e, ed) <- allEngines.reverse.zip(description.getChildren()))
      run(1, notifier, ed, e, Some(e))
    notifier.fireTestFinished(description)
  }

  def runChildren(depth: Int, notifier: RunNotifier, description: Description, r: Reportable, engine: Option[Engine]): Unit =
    r match {
      case holder: ReportableHolder => {
        val children = holder.children.zip(description.getChildren())
        for ((c, cd) <- children)
          run(depth + 1, notifier, cd, c, engine)
      }
      case _ =>
    }

  private def msg(depth: Int, activity: String, reportable: Reportable, description: Description) = {
    val dodgy = if (biMap._1(reportable) != description) "dodgy " + (reportable match { case r: Requirement => r.titleOrDescription("<NOTITLE>"); case _ => "" }) else ""
    String.format(s"%-${10 + depth}s  %-20s %s %s", activity, Reportable.templateName(reportable), description, dodgy)
  }

  def run(depth: Int, notifier: RunNotifier, description: Description, reportable: Reportable, engine: Option[Engine]) {
    test {
      if (CddRunner.needToAddListener.getAndSet(false)) {
        notifier.addListener(new RunListener() {
          override def testRunFinished(result: Result) = {
            println("JUnit Finished")
            recordEngines(CddRunner.enginesInTest.get)
          }
        })
      }
      notifier.fireTestStarted(description)
      log(msg(depth, "started", reportable, description))
      try {
        (engine, reportable) match {
          case (_, engine: EngineBuiltFromTests[_]) => runChildren(depth, notifier, description, engine, Some(engine))
          case (_, t: Test) if exceptionMap.contains(t) =>
            throw exceptionMap(t)
          case (Some(e: EngineBuiltFromTests[_]), t: Test) => {
            val actual = ROrException.from(e.applyParams(t.params))
            if (t.expected != Some(actual))
              throw new AssertionFailedError("Expected:\n" + t.expected + "\nActual:\n" + actual + "\n" + t)
          }
          case _ => runChildren(depth, notifier, description, reportable, engine)
        }
        log(msg(depth, "finished", reportable, description))
        notifier.fireTestFinished(description)
      } catch {
        case t: Throwable =>
          log(msg(depth, "failed", reportable, description))
          notifier.fireTestFailure(new Failure(description, t))
      }
    }
  }

  def returnTypeIsEngine(m: Method): Boolean = {
    val rt = m.getReturnType()
    return isEngine(rt)
  }

  def typeIsEngine(f: Field): Boolean = {
    val rt = f.getType()
    return isEngine(rt)
  }

  def isEngine(rt: Class[_]): Boolean = {
    val c = classOf[Engine]
    if (c.isAssignableFrom(rt))
      return true;
    for (t <- rt.getInterfaces())
      if (c.isAssignableFrom(t))
        return true;
    return false;
  }

  def instantiate(clazz: Class[_]): Any = {
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val declaredFields = clazz.getDeclaredFields().toList
    val moduleField = declaredFields.find(field => field.getName() == "MODULE$")
    try {
      val obj = moduleField match {
        case Some(modField) => modField.get(clazz)
        case None => clazz.newInstance()
      }
      obj
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Class: $clazz Field: $moduleField", e);
    }
  }
  def recordEngines(engines: List[Engine]) {
    import EngineWithLogger._
    engines.headOption match {
      case Some(engine) =>
        val packageObject = clazz.getPackage()
        val packageName = if (packageObject == null) "default package" else packageObject.getName()
        val project = Project("JUnit", engines: _*)
        val report = Report("Reports", project)
        ReportCreator.fileSystem(engine.logger, report).create
      case _ =>
    }
  }

  //  def recordEngine(clazz: Class[Any], logger: TddLogger) {
  //    import EngineWithLogger._
  //    val project = Project(clazz.getPackage().getName() + "/" + clazz.getSimpleName(), allEngines: _*)
  //    val report = Report("JUnit", project)
  //    ReportCreator.fileSystem(logger, report).create
  //  }
}

class CddJunitRunner(val clazz: Class[Any]) extends CddRunner {
  import org.cddcore.engine.Engine._

  def title = "CDD: " + clazz.getName
  val instance = test { instantiate(clazz) };

  val rootEnginesAndNames =
    test {
      clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m)).map((m) => (m.invoke(instance).asInstanceOf[Engine], m.getName)) ++
        clazz.getFields().filter((f) => typeIsEngine(f)).map((f) => (f.get(instance).asInstanceOf[Engine], f.getName)).sortBy(_._2)
    }

  val enginesToNameMap = Map(rootEnginesAndNames: _*)
  if (logging) {
    println(clazz)
    for ((name, engine) <- rootEnginesAndNames)
      println("Engine: " + name)
  }

  val rootEngines = rootEnginesAndNames.map(_._1)
  CddRunner.addToEngines(rootEngines.toList)

  object Main {
    trait SomeTrait { def someMethod: String }
    object SomeObject extends SomeTrait { def someMethod = "something" }

    class SomeClass extends SomeTrait { def someMethod = "something" }
    def main(args: Array[String]) = {
      val someClassTrait: SomeTrait = Class.forName("SomeClass").newInstance().asInstanceOf[SomeTrait]
      //      println("calling someClassTrait: " + someClassTrait.someMethod)
      val objectName = "SomeObject$"
      val cons = Class.forName(objectName).getDeclaredConstructors();
      cons(0).setAccessible(true);
      val someObjectTrait: SomeTrait = cons(0).newInstance().asInstanceOf[SomeTrait]
      //      println("calling someObjectTrait: " + someObjectTrait.someMethod)
    }
  }
}