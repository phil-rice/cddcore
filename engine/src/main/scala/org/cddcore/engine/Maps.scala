package org.cddcore.engine

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.CountDownLatch

class Maps {

}
object Maps {

  def addToList[K, V](map: Map[K, List[V]], key: K, value: V) = map + (key -> (value :: map.getOrElse(key, Nil)))

  def addToMapOfMapOfList[K1, K2, V](map: Map[K1, Map[K2, List[V]]], key1: K1, key2: K2, value: V) = {
    val map2: Map[K2, List[V]] = map.getOrElse(key1, Map())
    map + (key1 -> (map2 + (key2 -> (value :: map2.getOrElse(key2, List())))))
  }

  def walkSelfAndChildrenPaths[KV](map: Map[KV, List[KV]])(root: KV) = {
    def fn(root: KV): Iterable[List[KV]] = {
      val children = map.getOrElse(root, Nil)
      (List(List()) ++ children.flatMap(fn(_))).map(root :: _)
    }
    fn(root)
  }

  private val monitor = new Object

  class SettableFuture[V] extends Future[V] {
    private val latch = new CountDownLatch(1)
    var value: Option[V] = None
    var isCancelled = false;
    def cancel(mayInterruptIfRunning: Boolean) = throw new IllegalStateException
    def isDone = latch.getCount() == 0;
    def set(v: V) = { value = Some(v); latch.countDown() }
    def get(time: Long, unit: TimeUnit) = { latch.await(time, unit); value.get }
    def get = { latch.await; value.get }
  }

  /**  Making a thread safe cache is a little hard. This should only call the value method once, and all other calls wait until it is calculated. It uses a lock, but this lock doesn't need to be in a lock order, as no external code is called during the lock  */
  def getOrCreate[K, V](ref: AtomicReference[Map[K, Future[V]]], key: K, value: => V): V = {
    val map = ref.get()
    map.get(key) match {
      case Some(f) => f.get
      case _ =>
        val optFuture = monitor.synchronized {
          val map = ref.get()
          map.get(key) match {
            case Some(f) => None
            case _ =>
              val future = new SettableFuture[V]
              ref.set(map + (key -> future))
              Some(future)
          }
        }
        optFuture match {
          case Some(f) => { f.set(value); value }
          case _ => map(key).get
        }
    }
  }
}