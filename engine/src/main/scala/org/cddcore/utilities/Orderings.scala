package org.cddcore.utilities

object Orderings {

  def natural[X]: Ordering[X] = new Ordering[X] {
    def compare(x1: X, x2: X) = (x1, x2) match {
      case (i1: Int, i2: Int) => i1 compare i2
      case (s1: String, s2: String) => s1 compare s2
      case (a1: Any, a2: Any) => a1.toString compare a2.toString
    }
  }
}