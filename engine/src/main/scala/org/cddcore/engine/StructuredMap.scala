package org.cddcore.engine

class DuplicateKeyException(key: Key) extends Exception(key.toString)

object StructuredMap {
  def apply[V](kvs: (String, V)*) = kvs.foldLeft(new StructuredMap[V](new SimpleKeyStrategy))((acc, kv) => {
    val result = acc.put(kv._1, kv._2)
    result
  })
}

/** This class is designed to support the production of documents with keys like 1.1, 1.2.a... etc. The data (V) for the keys (String) can be added in any order.*/
class StructuredMap[V](keyStrategy: KeyStrategy, root: DataAndChildren[V] = DataAndChildren[V](Key("", List()), None, List())) {
  import scala.language.implicitConversions
  implicit def stringToKey(key: String) = keyStrategy.newKey(key)

  def put(key: String, value: V): StructuredMap[V] = new StructuredMap(keyStrategy, put(0, key, root, value))
  def get(key: String): Option[V] = get(0, key, root)
  def apply(key: String) = get(key) match { case Some(v) => v; case _ => throw new NoSuchElementException(key) }
  def walk(fn: (String, Option[V]) => Unit): Unit = walk(root, fn)
  def fold[Acc](initial: Acc)(fn: (Acc, String, Option[V]) => Acc): Acc = fold(root, initial, fn)
  def map[NewV](itemFn: (String, Option[V], List[NewV]) => NewV): NewV = map(root, itemFn)

  protected def map[NewV](node: DataAndChildren[V], itemFn: (String, Option[V], List[NewV]) => NewV): NewV = {
    val children = node.children
    val newChildren = children.map((c) => 
      map(c, itemFn))
    val result = itemFn(node.key.key, node.data, newChildren)
    result
  }

  protected def put(depth: Int, key: Key, node: DataAndChildren[V], value: V): DataAndChildren[V] = {
    if (node.key.path.length != depth)
      throw new IllegalStateException("Key: " + node.key + " depth: " + depth)
    if (node.key == key)
      if (node.data == None) node.copy(data = Some(value)) else
        throw new DuplicateKeyException(key)
    else {
      val index = node.children.indexWhere((dac) => dac.key.path(depth) == key.path(depth))
      index match {
        case -1 => {
          val newNode = if (key.path.length == depth + 1)
            DataAndChildren(key, Some(value), List())
          else {
            DataAndChildren(keyStrategy.newKeyAtDepth(key, depth + 1), None, List(
              put(depth + 2, key, DataAndChildren(keyStrategy.newKeyAtDepth(key, depth + 2), None, List()), value)))
          }

          node.copy(children = (newNode :: node.children).sorted(KeyOrder[V](depth)))
        }
        case _ =>
          node.copy(children = node.children.updated(index, put(depth + 1, key, node.children(index), value)))
      }
    }
  }

  protected def get(depth: Int, key: Key, node: DataAndChildren[V]): Option[V] = {
    if (node.key.path.length != depth) throw new IllegalStateException("Key: " + node.key + " depth: " + depth)
    if (node.key == key) return node.data
    val optChild = node.children.find((dac) => dac.key.path(depth) == key.path(depth))
    optChild match {
      case Some(child) =>
        get(depth + 1, key, child)
      case _ => None
    }
  }

  protected def walk(node: DataAndChildren[V], fn: (String, Option[V]) => Unit) {
    fn(node.key.key, node.data)
    for (c <- node.children)
      walk(c, fn)
  }
  protected def fold[Acc](node: DataAndChildren[V], initial: Acc, fn: (Acc, String, Option[V]) => Acc): Acc = {
    val acc = fn(initial, node.key.key, node.data)
    val result = node.children.foldLeft(acc)((acc, dac) => fold(dac, acc, fn))
    result
  }
}

case class Key(key: String, path: List[String])

case class DataAndChildren[V](key: Key, data: Option[V], children: List[DataAndChildren[V]])

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
  def apply(key: String): List[String] = key.split("\\.").map(_.trim).filter(_.length >0).toList
  def newKey(key: String): Key = {
    val newPath = apply(key)
    Key(newPath.mkString("."), newPath);
  }
  def newKeyAtDepth(key: Key, depth: Int): Key = {
    val newPath = key.path.take(depth);
    Key(newPath.mkString("."), newPath);
  }
}

case class KeyOrder[V](depth: Int) extends Ordering[DataAndChildren[V]] {
  def compare(x: DataAndChildren[V], y: DataAndChildren[V]): Int = {
    val xVal = x.key.path(depth)
    val yVal = y.key.path(depth)
    (Strings.safeToInt(xVal), Strings.safeToInt(yVal)) match {
      case (Some(xv), Some(yv)) => xv - yv
      case _ => xVal.compareTo(yVal)
    }
  }
}

