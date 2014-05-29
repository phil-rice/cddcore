package org.cddcore.utilities

import org.cddcore.engine.EngineException
import org.cddcore.engine.CannotSendNoneToOptionLens

trait Lens[A, B] extends Immutable {
  def get: A => B
  def set: (A, B) => A
  def apply(whole: A): B = get(whole)
  def mod(a: A, f: B => B): A = set(a, f(get(a)))
  def description: Option[String]
  def andThen[C](that: Lens[B, C]) = that compose this
  def compose[C](that: Lens[C, A]) = Lens[C, B](
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b)),
    Some(that.description.getOrElse("<?>") + "." + description.getOrElse("<?>")))
}

object Lens {
  def apply[A, B](get: A => B, set: (A, B) => A, description: Option[String] = None): Lens[A, B] = new SimpleLens[A, B](get, set, description)
  def option[A, B](get: A => Option[B], rawSet: (A, Option[B]) => A, exception: (B, B) => EngineException, description: Option[String] = None, validate: Option[(A, A, B) => Unit] = None) =
    new OptionLens[A, B](get, rawSet, exception, description, validate)
}
class SimpleLens[A, B](val get: A => B, val set: (A, B) => A, val description: Option[String]) extends Lens[A, B] {
  override def toString = description.collect { case d: String => s"Lens($d)" }.getOrElse(super.toString)
}

class OptionLens[A, B](val get: A => Option[B], rawSet: (A, Option[B]) => A, exception: (B, B) => EngineException, val description: Option[String],
  validate: Option[(A, A, B) => Unit]) extends Lens[A, Option[B]] {
  def set: (A, Option[B]) => A = (a, b) => {
    val newValue = b.getOrElse(throw CannotSendNoneToOptionLens(this))
    val result = get(a) match {
      case Some(old) => throw exception(old, newValue)
      case _ => rawSet(a, b)
    }
    validate.collect { case fn => fn(a, result, newValue) }
    result
  }

}

