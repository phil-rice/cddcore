package org.cddcore.structure

import scala.xml.NodeSeq
import scala.xml.Elem
import org.joda.time.DateTime
import scala.concurrent.stm._
import org.joda.time.format.DateTimeFormat

object Xml {
  type XmlFragment[T, A] = Fragment[Elem, NodeSeq, T, A]
  class XmlFragStrategy extends FragStrategy[Elem, NodeSeq] {
    def findResult(raw: Elem, paths: List[Path]) = paths.reverse.foldLeft(raw.asInstanceOf[NodeSeq])((acc, p) => p.linked match {
      case true => acc \ p.element
      case false => acc \\ p.element
    })
    def findA[T, A](result: NodeSeq, convertor: (NodeSeq) => Option[T], fold: Fold[T, A]): Option[A] = {
      val converted = result.map(convertor)
      converted.find((f) => f == None) match {
        case Some(thereIsANone) => None
        case _ => Some(converted.collect { case Some(x) => x }.foldLeft(fold.initial)(fold.fn))
      }
    }

  }

  val xmlFragStrategy = new XmlFragStrategy();

  def xml(x: Elem): Fragment[Elem, NodeSeq, _, _] = Fragment(xmlFragStrategy, x, List(), None)

  def optionDate: (NodeSeq) => Option[Option[DateTime]] = optionDate("yyyy-MM-dd")
  def optionDate(pattern: String): (NodeSeq) => Option[Option[DateTime]] = (n: NodeSeq) =>
    try { Some(Some(DateTimeFormat.forPattern(pattern).parseDateTime(n.text))) } catch {
      case e: Throwable => Some(None)
    };
  def date: (NodeSeq) => Option[DateTime] = date("yyyy-MM-dd")
  def date(pattern: String) = (n: NodeSeq) =>
    try { Some(DateTimeFormat.forPattern(pattern).parseDateTime(n.text)) } catch {
      case e: Throwable => None
    };

  def string = (n: NodeSeq) => Some(n.text)
  def integer = (n: NodeSeq) => Some(n.text.toInt)
  def double = (n: NodeSeq) => Some(n.text.toDouble)
  def nodeSeq = (n: NodeSeq) => Some(n)
  def list[T]() = Fold[T, List[T]](List[T](), (acc, t) => t :: acc)
  def yesNo: (NodeSeq) => Option[Boolean] = (n: NodeSeq) => n.text match {
    case "yes" => Some(true)
    case "no" => Some(false)
    case _ => None
  }
  def boolean: (NodeSeq) => Option[Boolean] = (n: NodeSeq) => n.text.toLowerCase() match {
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }
  def yesNo(default: Boolean): (NodeSeq) => Option[Boolean] = (n: NodeSeq) => n.text match {
    case "yes" => Some(true)
    case "no" => Some(false)
    case "" => Some(default)
    case _ => None
  }
  def obj[A](fn: (NodeSeq) => A) = (n: NodeSeq) => Some(fn(n))

  def main(args: Array[String]) {
    val x = <x>
              <a>1</a>
              <a>2</a>
              <a>3</a>
              <b>1</b>
            </x>
    val f1 = xml(x) \ "b" \ integer
    val f2 = xml(x) \ "a" \ integer \ Fold[Int, Int](0, (a, b) => a + b)
    println(f1())
    println(f2())
  }
}

trait XmlSituation extends Structure[Elem, NodeSeq] {
  type XmlFragment = Fragment[Elem, NodeSeq, _, _]

  private lazy val fragmentFields = new ClassFieldSet[XmlFragment](this, classOf[XmlFragment])
  private lazy val xmlFields = new ClassFieldSet[Elem](this, classOf[Elem])
  lazy val pathMap = PathMap[Elem, NodeSeq](fragmentFields.values)

  protected lazy val fragmentsToString = findFragmentsToString(fragmentFields.fieldMap, (e) => e.mkString(","))
  protected lazy val xmlsToString = structuresToString(pathMap, (s) => {
    val f = xmlFields.findFieldWithValue(s) match {
      case Some(f) => f.getName + "\n"
      case _ => ""
    }
    "  Xml: " + f + "  " + s.toString.replace('\n', ' ') + "\n"
  })

  override def toString() = {
    getClass.getSimpleName() + s"(\n  ${fragmentsToString}\n${xmlsToString})"
  }

  def htmlDisplay = s"<div class='xmlSituation'><span class='XmlTitle'>${getClass.getSimpleName}</span>" +
    "<div class='xmlSituationBody'>" +
    "<div class='xmlFound'><table>" +
    findFragmentsToString(fragmentFields.fieldMap, (e) => e.mkString(","), (f, a) => raw"<tr><td>${f.getName}</td><td> $a</td></tr>") +
    annotatedFieldsToString((name, value) => raw"<tr><td>$name</td><td> $value</td></tr>") +
    "</table></div><!--xmlFound -->\n" +
    //    "<div class='xmlFields'>" +
    //    structuresToString(pathMap, (s) => {
    //      val f = xmlFields.findFieldWithValue(s) match {
    //        case Some(f) => f.getName
    //        case _ => ""
    //      }
    //      "<div class='XmlTitle' title='" + Strings.htmlTooltipEscape(s.toString) + "'>" + Strings.htmlEscape(f) + "</div><!--XmlTitle'-->\n"
    //    }, separator = "<br />") +
    //    "</div><!--xmlFields -->\n" +
    "</div><!--xmlSituationBody -->\n" +
    "</div><!--xmlSituation -->\n"

}