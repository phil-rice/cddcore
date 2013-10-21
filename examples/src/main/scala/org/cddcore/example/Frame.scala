package org.cddcore.example

sealed abstract class Frame(val first: Int, val second: Int, val third: Int = 0, val size: Int = 2) {
  def score = first + second + third;
}

case class NormalFrame(f: Int, s: Int) extends Frame(f, s);
case class SpareFrame(f: Int, s: Int, t: Int) extends Frame(f, s, t);
case class StrikeFrame(f: Int, s: Int, t: Int) extends Frame(f, s, t, 1);