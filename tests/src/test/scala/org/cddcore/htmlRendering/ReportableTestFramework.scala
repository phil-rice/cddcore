package org.cddcore.htmlRendering

import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.utilities._

trait ReportableTestFramework {
  implicit def stringToOption(s: String) = Some(s)
  case class Holder(val name: String, val nodes: List[Reportable], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable

  case class ReqAndHolder(val name: String, val nodes: List[Reportable], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Requirement {
    def title: Option[String] = Some(name)
    def description: Option[String] = Some("desc_" + name)
    def priority: Option[Int] = None
    def references: Set[org.cddcore.engine.Reference] = Set()
    def copyRequirement(title: Option[String] = title,
      description: Option[String] = description,
      priority: Option[Int] = priority,
      references: Set[Reference] = references): Requirement = ???
  }
  case class Req(val title: Option[String], description: Option[String], references: Set[Reference] = Set(), priority: Option[Int] = None, textOrder: Int = Reportable.nextTextOrder) extends Requirement {
    def copyRequirement(title: Option[String] = title,
      description: Option[String] = description,
      priority: Option[Int] = priority,
      references: Set[Reference] = references): Requirement = ???

  }

  val repEmpty = Req("", "")
  val rep1 = Req("rep1", "d1")
  val rep1a = Req("rep1", "d1a")
  val rep1b = Req("rep1", "d1b")

  val rep2 = Req("rep2", "d2")
  val rep3 = Req("rep3", "d3")
  val rep4 = Req("rep4", "d4")
  val rep5 = Req("rep5", "d5")

  val holder12 = Holder("holder12", List(rep1, rep2))
  val holder345 = Holder("holder345", List(rep3, rep4, rep5))
  val holder_12_345 = Holder("holder_12_345", List(holder12, holder345))

  val doc0 = Document(Some("name0"))
  val doc1 = Document(Some("name1"))
  val doc2 = Document(Some("name2"))

  val ref0 = Reference("r0", None)
  val ref1 = Reference("r1", Some(doc1))
  val ref2 = Reference("r2", Some(doc2))

  val rd0 = Req("rd", "d1", Set(ref0))
  val rd1 = Req("rd1", "d1", Set(ref1))
  val rd2 = Req("rd2", "d2", Set(ref2))
  val rd12 = Req("rd12", "d12", Set(ref1, ref2))
  val holderD12 = Holder("holder12", List(rd1, rd2))
  val reqHolderD12 = ReqAndHolder("reqHolderD12", List(rd1, rd2))

  val reqHolder12 = ReqAndHolder("reqHolder12", List(rep1, rep2))
  val reqHolder345 = ReqAndHolder("reqHolder345", List(rep3, rep4, rep5))
  val reqHolder_12_345 = ReqAndHolder("reqHolder_12_345", List(reqHolder12, reqHolder345))

}