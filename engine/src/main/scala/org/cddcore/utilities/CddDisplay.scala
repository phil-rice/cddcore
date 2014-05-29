package org.cddcore.utilities

import org.cddcore.engine.Titled
import org.cddcore.engine.Engine

trait CddDisplay {
  def plain(cdp: CddDisplayProcessor): String
  def html(cdp: CddDisplayProcessor): String = plain(cdp)
}

trait CddDisplayer[C] {
  def clazz: Class[C]
  def plain(cdp: CddDisplayProcessor, c: C): String
  def html(cdp: CddDisplayProcessor, c: C): String
}

abstract class PlainCddDisplayer[C](val clazz: Class[C]) extends CddDisplayer[C] {
  def plain(cdp: CddDisplayProcessor, c: C): String
  def html(cdp: CddDisplayProcessor, c: C): String = c match { case c: CddDisplay => c.html(cdp); case _ => plain(cdp, c) }
}

abstract class HtmlCddDisplayer[C](val clazz: Class[C]) extends CddDisplayer[C] {
}

object CddDisplayProcessor {
  implicit val cdp: CddDisplayProcessor = apply()
  def default = List(
    new PlainCddDisplayer(classOf[Engine]) { def plain(cdp: CddDisplayProcessor, e: Engine) = s"${e.getClass().getSimpleName()}(${e.titleString})" },
    new HtmlCddDisplayer(classOf[List[_]]) {
      def plain(cdp: CddDisplayProcessor, l: List[_]) = "List(" + l.map(cdp.plain(_)).mkString(",") + ")"
      def html(cdp: CddDisplayProcessor, l: List[_]) = "List(" + l.map(cdp.html(_)).mkString(",") + ")"
    },
    new HtmlCddDisplayer(classOf[Tuple2[_, _]]) {
      def plain(cdp: CddDisplayProcessor, t2: Tuple2[_, _]) = s"(${cdp.plain(t2._1)},${cdp.plain(t2._2)})"
      def html(cdp: CddDisplayProcessor, t2: Tuple2[_, _]) = s"(${cdp.html(t2._1)},${cdp.html(t2._2)})"
    },
    new HtmlCddDisplayer(classOf[Tuple3[_, _, _]]) {
      def plain(cdp: CddDisplayProcessor, t3: Tuple3[_, _, _]) = s"(${cdp.plain(t3._1)},${cdp.plain(t3._2)},${cdp.plain(t3._3)})"
      def html(cdp: CddDisplayProcessor, t3: Tuple3[_, _, _]) = s"(${cdp.html(t3._1)},${cdp.html(t3._2)},${cdp.html(t3._3)})"
    })

  def apply(cds: CddDisplayer[_]*): CddDisplayProcessor = new SimpleCddDisplayProcessor(cds.toList ::: default)
  def raw(cds: CddDisplayer[_]*): CddDisplayProcessor = new SimpleCddDisplayProcessor(cds.toList)
}

trait CddDisplayProcessor extends Function[Any, String] {
  def +(c: CddDisplayer[_]): CddDisplayProcessor
  def displayerList: List[CddDisplayer[_]]
  def useDisplayer[C](a: Any, fn: (CddDisplayer[Any], Any) => String): Option[String] =
    displayerList.collectFirst { case cd: CddDisplayer[Any] if cd.clazz.isAssignableFrom(a.getClass) => fn(cd, a) }

  def apply(a: Any) = plain(a)

  def plain(a: Any): String = (useDisplayer(a, (cd, c) => cd.plain(this, c)), a) match {
    case (Some(p), _) => p
    case (None, d: CddDisplay) => d.plain(this)
    case (None, _) => a.toString
  }
  def html(a: Any): String = (useDisplayer(a, (cd, c) => cd.html(this, c)), a) match {
    case (Some(p), _) => p
    case (None, d: CddDisplay) => d.html(this)
    case (None, _) => a.toString
  }
}
case class SimpleCddDisplayProcessor(displayerList: List[CddDisplayer[_]] = List()) extends CddDisplayProcessor {
  def +(c: CddDisplayer[_]): CddDisplayProcessor = SimpleCddDisplayProcessor(c :: displayerList)

}
