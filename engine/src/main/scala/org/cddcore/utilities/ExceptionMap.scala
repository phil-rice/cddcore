package org.cddcore.utilities

import org.cddcore.engine.Reportable
import org.cddcore.engine.Requirement
import org.cddcore.engine.Engine
import org.cddcore.engine.Scenario
import scala.annotation.implicitNotFound

object ExceptionMap {
  def apply() = new ExceptionMap()
}
@implicitNotFound("No member of type class KeyLike in scope for ${T}")
trait KeyLike[-T] {
  def apply(t: T): Int
}
object KeyLike {
  implicit object ReportableLike extends KeyLike[Reportable] { def apply(r: Reportable): Int = r.textOrder }
}

class KeyedMapException(msg: String, val key: Any, val keyAsInt: Int, val map: KeyedMap[_], cause: Exception) extends Exception(msg, cause)
class KeyedMap[V](val map: Map[Int, V] = Map[Int, V]()) {
  def wrap[X](stuff: => X, error: (Exception) => Exception): X = try { stuff } catch { case e: Exception => throw error(e) }
  def wrapKey[T, X](key: T, stuff: => X)(implicit conv: KeyLike[T]): X = try { stuff } catch {
    case e: Exception =>
      throw new KeyedMapException(s"Key [${conv(key)}] was: $key\nMap:\n${map.mkString("\n")}", key, conv(key), this, e)
  }
  def apply[T](t: T)(implicit conv: KeyLike[T]) = wrapKey(t, map(conv(t)))
  def get[T](t: T)(implicit conv: KeyLike[T]) = wrapKey(t, map.get(conv(t)))
  def +[T](kv: (T, V))(implicit conv: KeyLike[T]) = kv match { case (t, v) => wrapKey(t, new KeyedMap[V](map + (conv(t) -> v))) }
  def ++(em: KeyedMap[V]) = new KeyedMap[V](em.map.foldLeft(map) { _ + _ })
  def contains[T](t: T)(implicit conv: KeyLike[T]) = wrapKey(t, map.contains(conv(t)))
  def size = map.size
  override def hashCode() = map.hashCode
  override def equals(other: Any) = other match { case e: ExceptionMap => e.map == map; case _ => false }
  override def toString = map.toString
}

class ExceptionMap(val map: Map[Int, List[Exception]] = Map()) {
  def apply[T](t: T)(implicit conv: KeyLike[T]) = map(conv(t))
  def get[T](t: T)(implicit conv: KeyLike[T]) = map.get(conv(t))
  def +[T](kv: (T, Exception))(implicit conv: KeyLike[T]) = kv match {
    case (t, e) => new ExceptionMap(Maps.addToList(map, conv(t), e))
  }
  def ++(em: ExceptionMap) = new ExceptionMap(em.map.foldLeft(map)((acc, kv) => kv._2.foldLeft(acc)((acc, v) => Maps.addToList(acc, kv._1, v))))
  def contains[T](t: T)(implicit conv: KeyLike[T]) = map.contains(conv(t))
  def size = map.size
  override def hashCode() = map.hashCode
  override def equals(other: Any) = other match { case e: ExceptionMap => e.map == map; case _ => false }

  def toMap[T](holder: NestedHolder[T])(implicit conv: KeyLike[T]): Map[T, List[Exception]] = {
    val keyToT = holder.foldLeft(Map[Int, T]())((acc, t) => acc + (conv(t) -> t))
    map.map((kv) => kv match { case (to, le) => keyToT(to) -> le }).foldLeft(Map[T, List[Exception]]()) { _ + _ }
  }
  override def toString = map.toString
}