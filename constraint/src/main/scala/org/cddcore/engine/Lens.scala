package org.cddcore.engine

trait LensTrait[A, B] extends Immutable {
  def get: A => B
  def set: (A, B) => A
  def apply(whole: A): B = get(whole)
  def mod(a: A, f: B => B): A = set(a, f(get(a)))
}
case class Lens[A, B](get: A => B, set: (A, B) => A) extends LensTrait[A, B] {
  def compose[C](that: LensTrait[C, A]) = Lens[C, B](
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b)))
  def andThen[C](that: Lens[B, C]) = that compose this
}

