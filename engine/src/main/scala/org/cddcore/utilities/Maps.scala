package org.cddcore.utilities

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.CountDownLatch

class Maps {
}

class MapToUniqueName[X](fn: (X, Int) => String, map: Map[X, String] = Map[X, String](), values: Set[String] = Set()) {
  def add(x: X): MapToUniqueName[X] = add(x, 1)

  def apply(x: X) = map(x)
  protected def add(x: X, count: Int): MapToUniqueName[X] = {
    val name = fn(x, count)
    values.contains(name) match {
      case true => add(x, count + 1)
      case false => new MapToUniqueName(fn, map + (x -> name), values + name)
    }
  }

}

object Maps {

  def addToList[K, V](map: Map[K, List[V]], key: K, value: V) = map + (key -> (value :: map.getOrElse(key, Nil)))

  def addToMapOfMapOfList[K1, K2, V](map: Map[K1, Map[K2, List[V]]], key1: K1, key2: K2, value: V) = {
    val map2: Map[K2, List[V]] = map.getOrElse(key1, Map())
    map + (key1 -> (map2 + (key2 -> (value :: map2.getOrElse(key2, List())))))
  }
  //  def addMapsOfList[K, V](map1: Map[K, List[V]], map2: Map[K, List[V]]) =
  //    map2.foldLeft(map1)((acc, kv) => kv._2.foldLeft(acc)((acc, v) => addToList(acc, kv._1, v)))

  def walkSelfAndChildrenPaths[KV](map: Map[KV, List[KV]])(root: KV) = {
    def fn(root: KV): Iterable[List[KV]] = {
      val children = map.getOrElse(root, Nil)
      (List(List()) ++ children.flatMap(fn(_))).map(root :: _)
    }
    fn(root)
  }

  private val monitor = new Object

  class SettableFuture[V](key: Any) extends Future[V] {
    private val latch = new CountDownLatch(1)
    val settingThread = Thread.currentThread()
    var value: Option[V] = None
    var isCancelled = false;
    def cancel(mayInterruptIfRunning: Boolean) = throw new IllegalStateException
    def isDone = latch.getCount() == 0;
    def set(v: V) = { value = Some(v); latch.countDown() }
    def checkThread { if (latch.getCount() != 0 && Thread.currentThread() == settingThread) throw new IllegalStateException("Cannot call cache for key: " + key + " inside the 'value' of the cache. ") }
    def get(time: Long, unit: TimeUnit) = { checkThread; latch.await(time, unit); value.get }
    def get = { checkThread; latch.await; value.get }
    override def toString = "Future(@" + System.identityHashCode(this) + ", " + latch.getCount() + ", " + value + ")"
  }

  /**  Making a thread safe cache is a little hard. This should only call the value method once, and all other calls wait until it is calculated. It uses a lock, but this lock doesn't need to be in a lock order, as no external code is called during the lock  */
  def getOrCreate[K, V](ref: AtomicReference[Map[K, Future[V]]], key: K, value: => V): V = {
    val map1 = ref.get()
    map1.get(key) match {
      case Some(f) =>
        f.get
      case _ =>
        val optFuture = monitor.synchronized {
          val map2 = ref.get() // just in case map was updated between map1 and start of synchronized block
          map2.get(key) match {
            case Some(f) => None
            case _ =>
              val future = new SettableFuture[V](key)
              ref.set(map2 + (key -> future))
              Some(future)
          }
        }
        //Note that the value calculation is done outside the lock. So this shouldn't cause any deadlocks

        val map3 = ref.get()
        optFuture match {
          case Some(f) => {
            val v = value
            f.set(v);
            map3(key).get
          }
          case _ => map3(key).get
        }
    }
  }
}