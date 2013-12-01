package org.cddcore.engine

import org.apache.log4j.Logger
import org.apache.log4j.Level

trait LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor): String
}

case class ClassFunction[C, T](clazz: Class[C], fn: (C) => T) {
  def apply(c: C) = fn(c)
}

case class ClassFunctionList[T](list: List[ClassFunction[_, T]] = List()) {
  def apply[C](c: C) =
    list.collectFirst({ case ClassFunction(clazz, f) if clazz.isAssignableFrom(c.getClass) => f.asInstanceOf[(C) => T](c.asInstanceOf[C]) })
  def getOrElse[C](c: C, default: => T): T =
    apply(c).getOrElse(default)

}

trait LoggerDisplayProcessor extends Function[Any, String] {
  def displayMap: ClassFunctionList[String]
  def apply(a: Any): String =
    displayMap.getOrElse(a,
      a match {
        case d: LoggerDisplay => d.loggerDisplay(this);
        case a => a.toString
      })
}

object LoggerDisplayProcessor {
  def apply(cf: ClassFunction[_, String]*) = new SimpleLoggerDisplayProcessor(ClassFunctionList(cf.toList))
}

object TddLogger {
  def noLogger = new NoLogger()
  val logger = Logger.getLogger(classOf[TddLogger]);
  def loggerPriority = logger.getLevel()
  def log(priority: Level, msg: String) = logger.log(priority, msg);
  sealed abstract class TddMessageType(val name: String)
  case class Compile() extends TddMessageType("Compile")
  case class Run() extends TddMessageType("Run")
  val compile = new Compile();
  val run = new Run();
  var defaultLogger = noLogger

}

trait TddLogger extends LoggerDisplayProcessor {
  import TddLogger._
  def newRoot(description: String)
  def addingUnder(description: String, parentWasTrue: Boolean, parentDescription: String)
  def addScenarioForRoot(description: String)
  def addFirstIfThenElse(description: String)
  def addScenarioFor[B, RFn, R](description: String, code: CodeFn[B, RFn, R])

  def merge(parentDescription: String, childDescription: String, yesNo: Boolean)
  def mergeRoot(description: String)

  def executing(params: List[Any])
  def evaluating(because: List[Any], condition: Boolean)
  def result(result: Any)
}

class DelegatingLogger(val delegate: TddLogger) extends TddLogger {
  def displayMap: ClassFunctionList[String] = delegate.displayMap

  def newRoot(description: String) = delegate.newRoot(description)
  def addingUnder(description: String, parentWasTrue: Boolean, parentDescription: String) = delegate.addingUnder(description, parentWasTrue, parentDescription)
  def addScenarioForRoot(description: String) = delegate.addScenarioForRoot(description)
  def addFirstIfThenElse(description: String) = delegate.addFirstIfThenElse(description)
  def addScenarioFor[B, RFn, R](description: String, code: CodeFn[B, RFn, R]) = delegate.addScenarioFor(description, code)

  def merge(parentDescription: String, childDescription: String, yesNo: Boolean) = delegate.merge(parentDescription, childDescription, yesNo)
  def mergeRoot(description: String) = delegate.mergeRoot(description)

  def executing(params: List[Any]) = delegate.executing(params)
  def evaluating(because: List[Any], condition: Boolean) = delegate.evaluating(because, condition)
  def result(result: Any) = delegate.result(result)
}

trait SimpleTddLogger extends TddLogger {
  import TddLogger._
  /** The raw log message that writes the string to an output */
  protected def message(priority: Level, msgType: TddMessageType, message: => String)

  def newRoot(description: String) = message(Level.DEBUG, TddLogger.compile, "Adding " + description + " as new root")
  def addingUnder(description: String, parentWasTrue: Boolean, parentDescription: String) = message(Level.DEBUG, TddLogger.compile, "Adding " + description + " under " + (if (parentWasTrue) "yes" else "no") + " of node " + parentDescription)
  def addScenarioForRoot(description: String) = message(Level.DEBUG, TddLogger.compile, "Adding " + description + " as extra scenario for root")
  def addFirstIfThenElse(description: String) = message(Level.DEBUG, TddLogger.compile, "Adding " + description + " as first if then else")
  def addScenarioFor[B, RFn, R](description: String, code: CodeFn[B, RFn, R]) = message(Level.DEBUG, TddLogger.compile, "Adding " + description + " as extra scenario for " + code.description)

  def merge(parentDescription: String, childDescription: String, yesNo: Boolean) =
    message(Level.DEBUG, TddLogger.compile, "Merging " + childDescription + " under " + yesNoLookup(yesNo) + " of " + parentDescription)
  def mergeRoot(description: String) =
    message(Level.DEBUG, TddLogger.compile, "Merging " + description + " into root")

  def executing(params: List[Any]) = message(Level.DEBUG, TddLogger.run, "Executing " + params.map(this).mkString(","))
  def evaluating(because: List[Any], condition: Boolean) = message(Level.INFO, TddLogger.run, " Condition " + because.mkString(" or ") + " was " + condition)
  def result(result: Any) =
    message(Level.DEBUG, TddLogger.run, " Result " + this(result))
  private val yesNoLookup = Map(false -> "no", true -> "yes")
}

class SimpleLoggerDisplayProcessor(val displayMap: ClassFunctionList[String] = ClassFunctionList()) extends LoggerDisplayProcessor

class Log4JLogger(override val displayMap: ClassFunctionList[String] = ClassFunctionList()) extends SimpleTddLogger {
  import TddLogger._

  protected def message(priority: Level, msgType: TddMessageType, message: => String) {
    if (priority.isGreaterOrEqual(loggerPriority))
      logger.log(priority, message)
  }

}

class TestLogger(override val displayMap: ClassFunctionList[String] = ClassFunctionList()) extends SimpleTddLogger {
  import TddLogger._
  private var list = List[String]()
  protected def message(priority: Level, msgType: TddMessageType, message: => String) {
    list = String.format("%-5s %-6s %s", priority, msgType, message) :: list
  }

  def messages = list.reverse
  def reset = list = List()

}

class ConsoleLogger(override val displayMap: ClassFunctionList[String] = ClassFunctionList()) extends SimpleTddLogger {
  import TddLogger._
  protected def message(priority: Level, msgType: TddMessageType, message: => String) =
    println(String.format("%-5s %-6s %s", priority, msgType, message))

}

class NoLogger extends SimpleTddLogger {
  import TddLogger._
  val displayMap: ClassFunctionList[String] = ClassFunctionList()
  protected def message(priority: Level, msgType: TddMessageType, message: => String) {}

}