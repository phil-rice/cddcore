package org.cddcore.utilities

import StartChildEndType._

object Lists {

  def increasingList[T](list: List[T]) = {
    list.foldLeft(List[List[T]]())((acc, t) => acc match {
      case h :: tail => (h :+ t) :: acc
      case _ => List(List(t))
    }).reverse
  }
  def decreasingList[T](list: Iterable[T]): List[List[T]] = list.toList match {
    case Nil => Nil;
    case l => decreasingListPrim(List(l), l).reverse
  }

  private def decreasingListPrim[T](acc: List[List[T]], list: List[T]): List[List[T]] =
    list match {
      case h :: Nil => acc
      case h :: t => decreasingListPrim(t :: acc, t)
      case _ => acc
    }

  def suffixSameCount[T](a: List[T], b: List[T]) = {
    val (suffixSameCount, _) = a.reverse.zip(b.reverse).foldLeft((0, true))((acc, lr) => (acc, lr) match {
      case ((value, true), (l, r)) if l == r => (value + 1, true)
      case _ => acc
    })
    suffixSameCount
  }

  /** this is only of value on paths. So paths are generated from things like the NestedHolder and have properties. They are a depth first traversal of a structure*/
  def traversableToStartChildEnd[T](traversable: Traversable[List[T]]) = {
    val paths = traversable.toList
    pathToStartChildEnd(paths)
    //    def closing(a: List[T], b: List[T]) = {
    //      val result = (a, Child) :: (1 to (a.size - suffixSameCount(a, b) - 1)).map((i) => (a.drop(i), End)).toList
    //      result
    //    }
    //    list.zipAll(list.tail, null, List()).flatMap {
    //      case (a, b) if a.size == b.size =>
    //        List((a, Child))
    //      case (a, b) if a.size < b.size =>
    //        List((a, Start))
    //      case (a, b) =>
    //        closing(a, b)
    //    }
  }
  def pathToStartChildEnd[T](paths: List[List[T]]) = {
    def closing(a: List[T], b: List[T]) = {
      val result = (a, Child) :: (1 to (a.size - suffixSameCount(a, b) - 1)).map((i) => (a.drop(i), End)).toList
      result
    }
    paths.zipAll(paths.tail, null, List()).flatMap {
      case (a, b) if a.size == b.size =>
        List((a, Child))
      case (a, b) if a.size < b.size =>
        List((a, Start))
      case (a, b) =>
        closing(a, b)
    }
  }
  def dumpPathsWithStartChildEnd[T](l: Traversable[(List[T], StartChildEndType)], fn: (T) => String = (t: T) => t.toString, pathSeparator: String = ",", separator: String = "\n"): String = {
    l.map { (x: (List[T], StartChildEndType)) =>
      x match {
        case (list, sce) => s"(${list.map(fn).mkString(pathSeparator)},$sce)"
      }
    }.mkString(separator)
  }

}