package org.cddcore.tests

import java.io.File
import java.lang.reflect.Field
import java.lang.reflect.Method
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import scala.Array.canBuildFrom
import org.cddcore.engine._
import org.cddcore.utilities._
import org.junit.runner.Description
import org.junit.runner.Runner
import org.junit.runner.notification.Failure
import org.junit.runner.notification.RunNotifier
import org.cddcore.engine.Engine.test
import org.cddcore.utilities.KeyLike.ReportableLike
import org.cddcore.htmlRendering.ReportOrchestrator
import org.cddcore.engine._

object CddRunner {
  val separator = "\n#########\n"
  val userHome = System.getProperty("user.home");
  var directory = new File("target", "cdd")
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

trait EngineWalker {
  def walk()
}



trait CddRunner extends Runner {
  type ExceptionOrEngine = Either[(Exception, String), EngineTools[_, _]]
  def instance: Either[Exception, Any]
  def ldp: CddDisplayProcessor = implicitly[CddDisplayProcessor]
  val templateLike = implicitly[TemplateLike[Reportable]]
  def clazz: Class[_ <: Any]
  val rootEngines: List[ExceptionOrEngine]
  var allEngines = List[Engine]()
  var reportableToDescription = new KeyedMap[Description]()
  var exceptionMap: ExceptionMap = new ExceptionMap()
  /** Descriptions should have unique names as JUnit has defined equals on it.  This helps avoid subtle bugs*/
  def makeDescriptionfor(r: Requirement): Description = {
    names = names.add(r)
    val name = names(r)
    val description = Description.createSuiteDescription(name)
    reportableToDescription = reportableToDescription + (r -> description)
    description
  }
  var names = new MapToUniqueName[Requirement]((r: Requirement, count: Int) => {
    val default = templateLike(r) + r.textOrder
    val name = Strings.clean(r match {
      case t: AnyScenario =>
        val result = t.titleOrDescription(ldp(t.toParams)) + " => " + ldp(t.toExpected.getOrElse(default))
        result
      case r: Requirement => r.title.getOrElse(default)
      case _ => throw new IllegalStateException(r.getClass() + "/" + r)
    })
    val result = count match { case 1 => name; case _ => name + "_" + count }
    result
  })

  def title: String

  def addEngine(exceptionOrEngine: ExceptionOrEngine) =
    exceptionOrEngine match {
      case Right(e) =>
        exceptionMap = exceptionMap ++ e.buildExceptions
        allEngines = e :: allEngines
      case _ =>
    }
  for (e <- rootEngines)
    addEngine(e)

  lazy val getDescription = {
    val result = Description.createSuiteDescription(title);
    instance match {
      case Left(e) =>
      case _ =>
        Engine.log("Running\n" + rootEngines.mkString("\n---------------------\n"))
        for (exceptionOrEngine <- rootEngines)
          exceptionOrEngine match {
            case Right(engine) => addRequirement(result, engine.asRequirement)
            case Left((exception, msg)) =>
          }
    }
    result
  }

  def addRequirement(d: Description, r: Requirement): Unit = {
    if (Engine.logging) Engine.log(s"Adding requirement: ${Strings.oneLine(r)}")
    val childDescription = makeDescriptionfor(r)
    val name = names(r)
    d.addChild(childDescription)
    r match {
      case holder: BuilderNodeHolder[_, _] => for (c <- holder.nodes) addRequirement(childDescription, c)
      case _ =>
    }
  }

  def run(notifier: RunNotifier) = {
    import KeyLike._
    import Strings.oneLine
    var depth = 0
    def indent = Strings.blanks(depth)
    def fail(d: Description, e: Exception) = {
      if (Engine.logging) { println(s"$indent Failed: "); e.printStackTrace() }
      notifier.fireTestFailure(new Failure(d, e))
    }

    def testStarted(description: Description) {
      if (Engine.logging) println(s"$indent Starting: ${oneLine(description)}")
      notifier.fireTestStarted(description);

    }
    def runDescription(description: Description, middle: => Unit) {
      depth += 1
      testStarted(description)
      try {
        middle
        notifier.fireTestFinished(description)
        if (Engine.logging) println(s"$indent Finishing: ${oneLine(description)}")
      } catch { case e: Exception => fail(description, e) }
      finally {
        depth -= 1
      }
    }

    def runReportable(r: Requirement, middle: => Unit) = {
      val description = reportableToDescription(r);
      if (description == null) {
        if (Engine.logging) println(s"$indent No description found for $r")
        throw new IllegalStateException(s"$indent No description found for $r")
      }
      exceptionMap.get(r) match {
        case Some(e :: Nil) => { testStarted(description); fail(description, e) }
        case Some(eList) => { testStarted(description); fail(description, MultipleScenarioExceptions(eList)) }
        case _ => runDescription(description, middle)
      }
    }
    def runReportableAndChildren[Params, R](r: Requirement, e: EngineFromTests[Params, R]): Unit =
      runReportable(r, r match {
        case holder: BuilderNodeAndHolder[Params, R] => for (child <- holder.nodes) runReportableAndChildren(child, e)
        case scenario: Scenario[Params, R] => {
          import e._;
          if (Engine.logging) println(s"$indent startRepAndChildren: ${oneLine(scenario.titleString)}")
          Exceptions(evaluator.validator.checkCorrectValue(evaluator, tree, scenario), (e) => s"Failed with $e")
          if (Engine.logging) println(s"$indent endRepAndChildren: ${oneLine(scenario.titleString)}")
        }
      })

    def runEngine[Params, R, FullR](e: EngineTools[Params, R]): Unit = e match {
      case d: DelegatedEngine[Params, R] => { runEngine(d.delegate); return }
      case f: FoldingEngine[Params, R, FullR] => runReportable(f.asRequirement, for (e <- f.engines) runEngine(e))
      case e: EngineFromTests[Params, R] => runReportableAndChildren(e.asRequirement, e)
    }

    Engine.log(s"Starting main run")

    val description = getDescription
    instance match {
      case Left(e) =>
        notifier.fireTestStarted(description)
        notifier.fireTestFailure(new Failure(description, e))
      case Right(i) =>
        runDescription(description, {
          for (e <- rootEngines)
            e match {
              case Left(e) =>
              case Right(e) => runEngine(e)
            }
        })
    }

    Engine.log(s"printing reports")
    CddRunner.directory.mkdirs()
    val orchestrator = new ReportOrchestrator(CddRunner.directory.toURL().toString(), "JUnit", allEngines)
    orchestrator.makeReports
    Engine.log(s"Ending main run")

  }
}

class CddJunitRunner(val clazz: Class[_]) extends CddRunner {
  import org.cddcore.engine.Engine._

  def title = "CDD: " + clazz.getName
  lazy val instance = Exceptions(test { Reflection.instantiate(clazz) });

  def instantiate(fn: (Any) => Array[ExceptionOrEngine]) = instance match {
    case Left(_) => Array[ExceptionOrEngine]()
    case Right(i) => fn(i)
  }

  lazy val methodRootEngines = instantiate((i) =>
    test { clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m)).map((m) => Exceptions(m.invoke(i).asInstanceOf[EngineTools[_, _]], { (_, m.getName) })) })
  lazy val variableRootEngines = instantiate((i) =>
    test { clazz.getFields().filter((f) => typeIsEngine(f)).map((f) => Exceptions(f.get(i).asInstanceOf[EngineTools[_, _]], { (_, f.getName) })) })

  lazy val rootEngines = { methodRootEngines ++ variableRootEngines }.sortBy { case (Right(e)) => e.asRequirement.textOrder; case Left(e) => 0 }.toList
  CddRunner.addToEngines(rootEngines.collect { case Right(e) => e })

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

trait HasEngines {
  def engines: List[Engine]
}

class CddContinuousIntegrationRunner(val clazz: Class[Any]) extends CddRunner {
  def title = "Constraint Driven Development"
  lazy val instance = Exceptions { Engine.test { Reflection.instantiate(clazz) }.asInstanceOf[HasEngines] };
  lazy val rootEngines = Engine.test { instance match { case Right(i) => i.engines.collect { case e: EngineTools[_, _] => Right(e) }; case _ => List() } }
}

trait CddContinuousIntegrationTest extends HasEngines {
  def engines: List[Engine]
}