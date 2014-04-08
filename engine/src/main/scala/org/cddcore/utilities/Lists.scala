package org.cddcore.utilities

import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

object Lists {
  implicit def toLists[A](l: List[A]) = new Lists(l)
}

class Lists[A](val delegate: List[A]) {
  /** Given List(1,2,3,4) will return List(List(1), List(1,2), List(1,2,3), List(1,2,3,4)) */
  def increasingList: List[List[A]] = decreasingList.reverse

  def decreasingList: List[List[A]] =
    delegate.foldLeft(List[List[A]]())((a, c) => (a match {
      case (h :: t) => (a.head :+ c) :: a;
      case _ => List(List(c))
    }))
}

