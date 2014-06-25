package org.cddcore.utilities

import scala.Option.option2Iterable

import scala.reflect.macros.Context
import scala.language.implicitConversions
import scala.language.experimental.macros

object CodeHolder {
  implicit def fnToHolder[Fn](fn: Fn): CodeHolder[Fn] = macro fnToHolder_impl[Fn]

  def fnToHolder_impl[Fn: c.WeakTypeTag](c: Context)(fn: c.Expr[Fn]): c.Expr[CodeHolder[Fn]] = {
    import c.universe._
    reify { CodeHolder[Fn](fn.splice, c.literal(show(fn.tree)).splice, "") }
  }

}
case class CodeHolder[Fn](val fn: Fn, val description: String, val comment: String = "") extends AbstractCodeHolder {
  //Need this to allow tests to pass. Obviously this is dodgy if we start putting them in maps etc... so don't!
  override def equals(other: Any): Boolean = other match {
    case c: CodeHolder[Fn] => c.description == description
    case _ => false
  }
  override def hashCode() = description.hashCode()
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

