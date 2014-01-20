package org.cddcore.engine

import scala.language.higherKinds
import scala.language.implicitConversions

class DuplicateKeyException(key: Key) extends Exception(key.toString)

/** This class is designed to support the production of documents with keys like 1.1, 1.2.a... etc. The data (V) for the keys (String) can be added in any order.*/
abstract class AbstractStructuredMap[V, I](keyStrategy: KeyStrategy, root: DataAndChildren[V, I])(implicit toIterable: (I) => Iterable[V]) {
  import scala.language.implicitConversions
  implicit def stringToKey(key: String) = keyStrategy.newKey(key)

  def walk(fn: (String, I) => Unit): Unit = walk(root, fn)
  def fold[Acc](initial: Acc)(fn: (Acc, String, I) => Acc): Acc = fold(root, initial, fn)

  protected def map[NewV](node: DataAndChildren[V, I], itemFn: (String, I, List[NewV]) => NewV): NewV = {
    val children = node.children
    val newChildren = children.map((c) =>
      map(c, itemFn))
    val result = itemFn(node.key.key, node.data, newChildren)
    result
  }

  protected def blankI: I
  protected def makeNewV(key: Key, oldI: I, v: V): I

  protected def put(depth: Int, key: Key, node: DataAndChildren[V, I], value: V): DataAndChildren[V, I] = {
    if (node.key.path.length != depth)
      throw new IllegalStateException("Key: " + node.key + " depth: " + depth)
    if (node.key == key)
      node.copy(data = makeNewV(key, node.data, value))
    else {
      val index = node.children.indexWhere((dac) => dac.key.path(depth) == key.path(depth))
      index match {
        case -1 => {
          val newNode = if (key.path.length == depth + 1)
            DataAndChildren[V, I](key, makeNewV(key, blankI, value), List())
          else {
            DataAndChildren(keyStrategy.newKeyAtDepth(key, depth + 1), blankI, List(
              put(depth + 2, key, DataAndChildren(keyStrategy.newKeyAtDepth(key, depth + 2), blankI, List()), value)))
          }

          node.copy(children = (newNode :: node.children).sorted(KeyOrder[V, I](depth)))
        }
        case _ =>
          node.copy(children = node.children.updated(index, put(depth + 1, key, node.children(index), value)))
      }
    }
  }

  protected def get(depth: Int, key: Key, node: DataAndChildren[V, I]): I = {
    if (node.key.path.length != depth) throw new IllegalStateException("Key: " + node.key + " depth: " + depth)
    if (node.key == key) return node.data
    val optChild = node.children.find((dac) => dac.key.path(depth) == key.path(depth))
    optChild match {
      case Some(child) =>
        get(depth + 1, key, child)
      case _ => blankI
    }
  }

  protected def walk(node: DataAndChildren[V, I], fn: (String, I) => Unit) {
    fn(node.key.key, node.data)
    for (c <- node.children)
      walk(c, fn)
  }
  protected def fold[Acc](node: DataAndChildren[V, I], initial: Acc, fn: (Acc, String, I) => Acc): Acc = {
    val acc = fn(initial, node.key.key, node.data)
    val result = node.children.foldLeft(acc)((acc, dac) => fold(dac, acc, fn))
    result
  }
}

object StructuredMap {
  protected implicit def optionToImplicit[V](i: Option[V]): Iterable[V] = i
  def apply[V](kvs: (String, V)*) = kvs.foldLeft(new StructuredMap[V](new SimpleKeyStrategy, new DataAndChildren(Key("", List()), None, List())))((acc, kv) => acc + kv)
}

class StructuredMap[V](keyStrategy: KeyStrategy, root: DataAndChildren[V, Option[V]]) extends AbstractStructuredMap[V, Option[V]](keyStrategy, root) {
  def +(keyValue: (String, V)): StructuredMap[V] = new StructuredMap(keyStrategy, put(0, keyValue._1, root, keyValue._2))
  def get(key: String): Option[V] = get(0, key, root)
  def apply(key: String) = get(key) match { case Some(v) => v; case _ => throw new NoSuchElementException(key) }
  def map[NewV](itemFn: (String, Option[V], List[NewV]) => NewV): NewV = map(root, itemFn)
  protected def blankI: Option[V] = None
  protected def makeNewV(key: Key, oldI: Option[V], v: V): Option[V] =
    if (oldI.isDefined)  
      throw new DuplicateKeyException(key.toString) 
    else Some(v)
     
}

object StructuredMapOfList {
  def apply[V](kvs: (String, V)*) = kvs.foldLeft(new StructuredMapOfList[V](new SimpleKeyStrategy, new DataAndChildren[V, List[V]](Key("", List()), List(), List())))((acc, kv) => acc + kv)
}
class StructuredMapOfList[V](keyStrategy: KeyStrategy, root: DataAndChildren[V, List[V]]) extends AbstractStructuredMap[V, List[V]](keyStrategy, root) {
  def get(key: String): List[V] = get(0, key, root)
  def +(kv: (String, V)): StructuredMapOfList[V] = {
    val (key, value) = kv
    new StructuredMapOfList(keyStrategy, put(0, key, root, value))
  }
  protected def blankI = List()
  protected def makeNewV(key: Key, oldI: List[V], v: V): List[V] = oldI :+ v

}

case class Key(key: String, path: List[String])

case class DataAndChildren[V, I](key: Key, data: I, children: List[DataAndChildren[V, I]])(implicit toIterable: (I) => Iterable[V])

trait KeyStrategy {
  def rootKey: String
  def findKeyFor(root: String, children: List[_], child: Any): String
  def apply(key: String): List[String]
  def newKey(key: String): Key
  def newKeyAtDepth(key: Key, depth: Int): Key
}

class SimpleKeyStrategy extends KeyStrategy {
  val rootKey = ""
  def findKeyFor(root: String, children: List[_], child: Any) = (root, children.indexOf(child) + 1) match {
    case (_, 0) => throw new IllegalStateException
    case ("", i) => i.toString
    case (root, i) => root + "." + i
  }
  def apply(key: String): List[String] = key.split("\\.").map(_.trim).filter(_.length > 0).toList
  def newKey(key: String): Key = {
    val newPath = apply(key)
    Key(newPath.mkString("."), newPath);
  }
  def newKeyAtDepth(key: Key, depth: Int): Key = {
    val newPath = key.path.take(depth);
    Key(newPath.mkString("."), newPath);
  }
}

case class KeyOrder[V, I](depth: Int) extends Ordering[DataAndChildren[V, I]] {
  def compare(x: DataAndChildren[V, I], y: DataAndChildren[V, I]): Int = {
    val xVal = x.key.path(depth)
    val yVal = y.key.path(depth)
    (Strings.safeToInt(xVal), Strings.safeToInt(yVal)) match {
      case (Some(xv), Some(yv)) => xv - yv
      case _ => xVal.compareTo(yVal)
    }
  }
}

