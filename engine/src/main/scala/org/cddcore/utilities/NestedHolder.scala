package org.cddcore.utilities

import org.cddcore.engine.Reportable
import org.cddcore.engine._
import StartChildEndType._



abstract class PathTraversable[T, X >: T](initialPath: List[X]) extends Traversable[List[X]] {
  protected def foreachPrim[U](nodes: List[T], f: List[X] => U, path: List[X]): Unit = {
    //    println(s"Path$path Nodes$nodes")
    for (c <- nodes) {
      val cPath = c :: path
      f(cPath)
      c match { case holder: NestedHolder[T] => foreachPrim(holder.nodes, f, cPath); case _ => }
    }
  }
}

trait NestedHolder[T] extends Traversable[T] {
  def nodes: List[T]

  def foreach[U](f: T => U): Unit = {
    for (c <- nodes) c match {
      case holder: NestedHolder[T] =>
        f(c); holder.foreach(f)
      case _ => f(c);
    }
  }
  def all[C](clazz: Class[C]) = this.collect { case c: Any if (clazz.isAssignableFrom(c.getClass())) => c.asInstanceOf[C] }.toList

  def pathsFrom[X >: T](initialPath: List[X]) = new PathTraversable[T, X](List()) {
    def foreach[U](f: List[X] => U) = foreachPrim[U](nodes, f, initialPath)
  }
  lazy val paths = pathsFrom[T](List())

  lazy val pathsIncludingSelf: Traversable[List[T]] = pathsIncludingSelf(List())
  def pathsIncludingSelf[X >: T](initialPath: List[X]) = new PathTraversable[T, X](initialPath) {
    def foreach[U](f: List[X] => U): Unit = {
      val initialValue = NestedHolder.this.asInstanceOf[T] :: initialPath
      f(initialValue)
      foreachPrim[U](nodes, f, initialValue)
    }
  }


}

case class SimpleNestedHolder[T](nodes: List[T]) extends NestedHolder[T]
