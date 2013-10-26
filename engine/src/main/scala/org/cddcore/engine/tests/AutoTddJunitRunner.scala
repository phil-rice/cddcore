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
trait NotActuallyFactory[R] extends EngineUniverse[R] {
  def builder: RealScenarioBuilder = ???
  def logger: org.cddcore.engine.TddLogger = TddLogger.noLogger
  def rfnMaker: scala.util.Either[() => Exception, Any] => RFn = ???
  def makeClosureForBecause(params: List[Any]) = ???
  def makeClosureForCfg(params: List[Any]) = ???
  def makeClosureForResult(params: List[Any]) = ???
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = ???

}

trait CddRunner extends Runner with EngineUniverse[Any] with NotActuallyFactory[Any] {

  var engineMap: Map[Description, Engine] = Map()
  var ScenarioMap: Map[Description, Scenario] = Map()
  var exceptionMap: Map[Description, Throwable] = Map()

  def addEngineForTest(name: String, engine: Any) = addEngine(name, engine.asInstanceOf[Engine])

  def addEngine(name: String, engine: Engine) = {
    val engineDescription = Description.createSuiteDescription(name)
    println(name)
    println(engine)
    add(engineDescription, engine)
    engineDescription
  }

  //  private var i = 0;

  def add(engineDescription: Description, engine: Engine) {
    getDescription.addChild(engineDescription)

    //    manipulator.append("\n\n" + <new(null, "pre", e>, false, pre>new }</pre>engine.constructionString }</pre>$buf_*) }</pre>)

    engineMap = engineMap + (engineDescription -> engine)

    for (u <- engine.useCases) yield {
      val useCaseDescription = Description.createSuiteDescription(Strings.clean(u.title.getOrElse("")))
      engineDescription.addChild(useCaseDescription)

      for (s <- u.scenarios) yield {
        val name = s.titleString + " => " + logger(s.expected.getOrElse("")) + " " + s.becauseString
        val cleanedName = Strings.clean(name)
        println("   " + cleanedName)
        val scenarioDescription = Description.createSuiteDescription(cleanedName)
        useCaseDescription.addChild(scenarioDescription)
        ScenarioMap = ScenarioMap + (scenarioDescription -> s.asInstanceOf[Scenario])

      }
    }
  }

  def fileFor(clazz: Class[Any], ed: Description, extension: String) = new File(CddRunner.directory, clazz.getName() + "." + ed.getDisplayName() + "." + extension)

  def saveResults(clazz: Class[Any], ed: Description, e: Engine) {
    import Files._
    printToFile(fileFor(clazz, ed, "dt.html"))((pw) => pw.write(e.toStringWith(new HtmlIfThenPrinter)))
    //        new File(AutoTddRunner.directory, clazz.getName() + "." + ed.getDisplayName() + ".attd"))((pw) => pw.write(e.toString))
  }

  def log(s: String) = println(s)

  def run(notifier: RunNotifier) {
    EngineTest.test(() => {
      notifier.fireTestStarted(getDescription)
      for (ed <- getDescription.getChildren) yield {
        log("notifier.fireTestStarted(ed)" + ed)
        notifier.fireTestStarted(ed)
        val engine = engineMap(ed)
        for (ud <- ed.getChildren) yield {
          log("notifier.fireTestStarted(ud)" + ud)
          notifier.fireTestStarted(ud)
          for (sd <- ud.getChildren) yield {
            log("notifier.fireTestStarted(sd)" + sd)
            notifier.fireTestStarted(sd)
            val scenario = ScenarioMap(sd)
            if (engine.scenarioExceptionMap.contains(scenario)) {
              log("notifier.fireTestFailure(sd)" + sd)
              notifier.fireTestFailure(new Failure(sd, engine.scenarioExceptionMap(scenario)))
            } else {
              //            val b = engine.makeClosureForBecause(Scenario.params);
              if (engine.root == null) {
                notifier.fireTestIgnored(sd)
                log("notifier.fireTestIgnored(sd)" + sd)
              } else
                try {
                  val actual = ROrException(engine.applyParam(engine.root, scenario.params, true))
                  if (scenario.expected == Some(actual)) {
                    log("notifier.fireTestFinished(sd)" + sd)
                    notifier.fireTestFinished(sd)
                  } else
                    throw new AssertionFailedError("Expected:\n" + scenario.expected + "\nActual:\n" + actual + "\n" + scenario)
                } catch {
                  //                  case e: AssertionFailedError => 
                  case e: Throwable =>
                    //                    e.printStackTrace()
                    val f = new Failure(sd, e)
                    log("notifier.fireTestFailure(sd)" + sd)
                    notifier.fireTestFailure(f)
                }
            }
          }
          log("notifier.fireTestFinished(ud)" + ud)
          notifier.fireTestFinished(ud)
        }
        log("notifier.fireTestFinished(ed)" + ed)
        notifier.fireTestFinished(ed)
        //        println("Scenarios for: " + ed.getDisplayName())
        //        for (c <- engine.Scenarios)
        //          println("  " + c)
      }
      log("notifier.fireTestFinished(getDescription)" + getDescription)
      notifier.fireTestFinished(getDescription)
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

  val getDescription = Description.createSuiteDescription("ATDD: " + clazz.getName);

  val instance = EngineTest.test(() => { instantiate(clazz) });

  override def addEngine(name: String, engine: Engine) = {
    val ed = super.addEngine(name, engine)
    recordEngine(clazz, ed, engine)
    ed
  }

  for (m <- clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m))) {
    val engine: Engine = m.invoke(instance).asInstanceOf[Engine];
    addEngine(m.getName(), engine)
  }
  for (f <- clazz.getFields().filter((f) => typeIsEngine(f))) {
    val engine: Engine = f.get(instance).asInstanceOf[Engine];
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

  def recordEngine(clazz: Class[Any], engineDescription: Description, engine: Engine) {
    val file = fileFor(clazz, engineDescription, "html")
    file.delete();
    Files.appendToFile(file)((p) => p.append(Report("Junit Result", engine).html()))

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