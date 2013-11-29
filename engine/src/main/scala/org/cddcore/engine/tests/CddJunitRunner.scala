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

object CddRunner {
  val separator = "\n#########\n"
  val userHome = System.getProperty("user.home");
  val directory = new File(userHome, ".cdd")

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
  type BiMap = Tuple2[Map[Reportable, Description], Map[Description, Reportable]]
  import org.cddcore.engine.Engine._
  var exceptionMap: Map[Test, Throwable] = Map()
  var biMap: BiMap = (Map(), Map())

  def addEngineForTest(name: String, engine: Any) = addEngine(name, engine.asInstanceOf[EngineFromTestsImpl])

  def addEngine(name: String, engine: EngineFromTestsImpl) = {
    val engineDescription = Description.createSuiteDescription(name)
    println(name)
    println(engine)
    getDescription.addChild(engineDescription)
    val result = engine.foldWithPathReversingChildren[BiMap](List(), (biMap._1 + (engine -> engineDescription), biMap._2 + (engineDescription -> engine)), (acc, path) => (acc, path) match {
      case ((from, to), (r: Requirement) :: tail) if !from.contains(r) =>
        val d = Description.createSuiteDescription(Strings.clean(r match {
          case t: Test => t.titleString + " => " + logger(t.expected.getOrElse(""))
          case _ => r.title.getOrElse("")
        }))
        tail.headOption match { case Some(parent) => from(parent).addChild(d); case _ => }
        (from + (r -> d), to + (d -> r))
      case _ => acc
    })
    biMap = result
    exceptionMap = exceptionMap ++ engine.scenarioExceptionMap.map
    engineDescription
  }

  def fileFor(clazz: Class[Any], ed: Description, extension: String) = new File(CddRunner.directory, clazz.getName() + "." + ed.getDisplayName() + "." + extension)

  def saveResults(clazz: Class[Any], ed: Description, e: EngineFromTestsImpl) {
    import Files._
    printToFile(fileFor(clazz, ed, "dt.html"))((pw) => pw.write(e.toStringWith(new HtmlIfThenPrinter)))
  }

  def log(s: String) = println(s)

  def run(notifier: RunNotifier) =
    run(notifier, getDescription(), None)

  def runChildren(notifier: RunNotifier, r: Reportable, engine: Option[Engine]): Unit =
    runChildren(notifier, biMap._1(r), engine)

  def runChildren(notifier: RunNotifier, description: Description, engine: Option[Engine]) =
    for (d <- description.getChildren)
      run(notifier, d, engine)

  def run(notifier: RunNotifier, description: Description, engine: Option[Engine]) {
    test(() => {
      notifier.fireTestStarted(description)
      log("notifier.fireTestStarted" + description)
      try {
        (engine, biMap._2.get(description)) match {
          case (_, Some(engine: EngineBuiltFromTests[_])) => runChildren(notifier, engine, Some(engine))
          case (_, Some(t: Test)) if exceptionMap.contains(t)=> throw exceptionMap(t)
          case (Some(e: EngineBuiltFromTests[_]), Some(t: Test)) => {
            val actual = ROrException.from(e.applyParams(t.params))
            if (t.expected != Some(actual)) 
              throw new AssertionFailedError("Expected:\n" + t.expected + "\nActual:\n" + actual + "\n" + t)
          }
          case (_, Some(x)) => runChildren(notifier, x, engine)
          case _ => runChildren(notifier, description, engine)
        }
        log("notifier.fireTestFinished" + description)
        notifier.fireTestFinished(description)
      } catch {
        case t: Throwable =>
          log("notifier.fireTestFailure" + description)
          notifier.fireTestFailure(new Failure(description, t))
      }
    })
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

}

class CddJunitRunner(val clazz: Class[Any]) extends CddRunner {
  import org.cddcore.engine.Engine._
  val getDescription = Description.createSuiteDescription("ATDD: " + clazz.getName);

  val instance = test(() => { instantiate(clazz) });

  override def addEngine(name: String, engine: EngineFromTestsImpl) = {
    val ed = super.addEngine(name, engine)
    recordEngine(clazz, ed, engine)
    ed
  }

  for (m <- clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m))) {
    val engine: EngineFromTestsImpl = m.invoke(instance).asInstanceOf[EngineFromTestsImpl];
    addEngine(m.getName(), engine)
  }
  for (f <- clazz.getFields().filter((f) => typeIsEngine(f))) {
    val engine: EngineFromTestsImpl = f.get(instance).asInstanceOf[EngineFromTestsImpl];
    addEngine(f.getName, engine)
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

  def recordEngine(clazz: Class[Any], engineDescription: Description, engine: EngineFromTestsImpl) {
    val project = Project("Junit_" + engine.titleOrDescription("Unnamed"), engine)
    ReportCreator.fileSystem(project).create
  }

  trait SomeTrait { def someMethod: String }
  object SomeObject extends SomeTrait { def someMethod = "something" }

  class SomeClass extends SomeTrait { def someMethod = "something" }

  object Main {
    def main(args: Array[String]) = {
      val someClassTrait: SomeTrait = Class.forName("SomeClass").newInstance().asInstanceOf[SomeTrait]
      println("calling someClassTrait: " + someClassTrait.someMethod)
      val objectName = "SomeObject$"
      val cons = Class.forName(objectName).getDeclaredConstructors();
      cons(0).setAccessible(true);
      val someObjectTrait: SomeTrait = cons(0).newInstance().asInstanceOf[SomeTrait]
      println("calling someObjectTrait: " + someObjectTrait.someMethod)
    }
  }
}