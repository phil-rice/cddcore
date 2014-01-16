package org.cddcore.engine

import scala.Option.option2Iterable
import scala.reflect.macros.Context
import scala.language.implicitConversions
import scala.language.experimental.macros

class EngineException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}
class InvalidBecauseException(msg: String) extends EngineException(msg, null)

case class Configurator[K](item: K, fn: (K) => Unit);

object RfnMaker {
  def rfn1ConstantMaker[P, R] = (e: Either[() => Exception, R]) => e match { case Left(e) => (p: P) => throw e(); case Right(r) => (p: P) => r }
  def rfn2ConstantMaker[P1, P2, R] = (e: Either[() => Exception, R]) => e match { case Left(e) => (p1: P1, p2: P2) => throw e(); case Right(r) => (p1: P1, p2: P2) => r }
  def rfn3ConstantMaker[P1, P2, P3, R] = (e: Either[() => Exception, R]) => e match { case Left(e) => (p1: P1, p2: P2, p3: P3) => throw e(); case Right(r) => (p1: P1, p2: P2, p3: P3) => r }

}

object CodeHolder {
  implicit def fnToHolder[Fn](fn: Fn): CodeHolder[Fn] = macro fnToHolder_impl[Fn]

  def fnToHolder_impl[Fn: c.WeakTypeTag](c: Context)(fn: c.Expr[Fn]): c.Expr[CodeHolder[Fn]] = {
    import c.universe._
    reify { CodeHolder[Fn](fn.splice, c.literal(show(fn.tree)).splice, "") }
  }

}

trait AbstractCodeHolder {
  def description: String
  def comment: String
  private val index = description.indexOf("=>");
  lazy val pretty = (index match {
    case -1 => description
    case i => description.substring(index + 3, description.length - 1)
  }).replace(".this.", ".").replace(".apply(", "(")
  //TODO Need better extraction of parameters as the parameters could be functions
  val parameters = index match {
    case -1 => description
    case i => description.substring(0, index);
  }

  override def toString = getClass.getSimpleName() + "(" + description + ")"
}
case class CodeHolder[Fn](val fn: Fn, val description: String, val comment: String = "") extends AbstractCodeHolder 


