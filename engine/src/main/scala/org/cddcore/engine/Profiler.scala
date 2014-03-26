package org.cddcore.engine

import java.util.ArrayList
import java.util.HashMap
import scala.collection.JavaConversions._

object Profiler {
  def prettifyFn[T] = (t:T ) => t match {case r: Requirement => r.titleString ; case _ => t.toString} 
}

trait Profiler[T] {
  def prettifyFn: (T) => String =Profiler.prettifyFn[T]
  def get(e: T): ProfilerRecord
  def results: Map[T, ProfilerRecord]
  def start(e: T) = startWithTime(System.nanoTime(), e)
  def end(e: T) = endWithTime(System.nanoTime(), e)
  def startWithTime(time: Long, e: T) = {
    val pr = get(e)
    pr.startTime.add(time)
  }

  def endWithTime(time: Long, e: T) = {
    val pr = get(e)
    pr.count += 1
    val index = pr.startTime.size - 1
    if (index == 0) {
      val duration = time - pr.startTime.get(index)
      pr.totalTime += duration
    }
    pr.startTime.remove(index)
  }

  def prettyString = results.map {
    case (e, pr) => s"${prettifyFn(e)} called ${pr.count} and took ${pr.totalTime}"
  }.mkString("\n")
}

class SimpleProfiler[T] extends Profiler[T] {
  var map = new HashMap[T, ProfilerRecord]
  def get(e: T) = {
    if (!map.containsKey(e))
      map.put(e, ProfilerRecord())
    map.get(e)
  }
  def results: Map[T, ProfilerRecord] = map.toMap
}
class ThreadedProfiler[T] extends Profiler[T] {
  private val monitor = new Object
  private var list = List[HashMap[T, ProfilerRecord]]()
  private val map = new ThreadLocal[HashMap[T, ProfilerRecord]]() {
    override def initialValue = monitor.synchronized { val result = new HashMap[T, ProfilerRecord](); list = result :: list; result }
  }

  def get(e: T) = {
    val myMap = map.get()
    if (myMap.containsKey(e))
      myMap.get(e)
    else {
      val pr = ProfilerRecord()
      myMap.put(e, pr)
      pr
    }
  }

  def results = {
    val copy = monitor.synchronized(list)
    copy.foldLeft(Map[T, ProfilerRecord]())((acc, v) => {
      var result = acc
      for ((e, pr) <- v) {
        if (!result.contains(e))
          result = result + (e -> ProfilerRecord())
        result(e).add(pr)
      }
      result
    })
  }
}

case class ProfilerRecord(
  var count: Int = 0,
  var totalTime: Long = 0,
  var startTime: ArrayList[Long] = new ArrayList()) {

  def add(pr: ProfilerRecord) {
    count += pr.count
    totalTime += pr.totalTime
  }
}
