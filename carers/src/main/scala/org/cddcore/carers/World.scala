package org.cddcore.carers

import org.joda.time.format.DateTimeFormat
import org.cddcore.engine.LoggerDisplay
import scala.concurrent.stm.InTxn
import org.apache.commons.dbcp.BasicDataSource
import scala.xml.Elem
import org.springframework.jdbc.core.JdbcTemplate
import scala.xml.XML
import org.joda.time.DateTime
import org.cddcore.engine.LoggerDisplayProcessor
import scala.concurrent.stm.Ref
import scala.language.implicitConversions

object Xmls {

  def validateClaim(id: String) = {
    try {
      val full = s"ValidateClaim/${id}.xml"
      val url = getClass.getClassLoader.getResource(full)
      val xmlString = scala.io.Source.fromURL(url).mkString
      val xml = XML.loadString(xmlString)
      xml
    } catch {
      case e: Exception => throw new RuntimeException("Cannot load " + id, e)
    }
  }

  //  lazy val ageUnder16 = validateClaim("CL100104A")
  //  lazy val lessThen35Hours = validateClaim("CL100105A")
  //  lazy val notInGB = validateClaim("CL100107A")
  //  lazy val dpWithoutLevelOfQualifyingBenefit = validateClaim("CL100106A")
  //  lazy val customerNotGbResidentAndResident = validateClaim("CL100105A")

  private val formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
  def asDate(s: String): DateTime = formatter.parseDateTime(s);
  def tagPresent(e: Elem, tag: String): Boolean = (e \ tag).size > 0
  def tagHasTextPresent(e: Elem, tag: String): Boolean = (e \ tag).text.size > 0
  def asNumber(s: String): Integer = Integer.parseInt(s)
  def asFloat(s: String): Double = java.lang.Double.parseDouble(s)

  def asYesNo(s: String): Boolean = {
    s match {
      case "yes" => true;
      case _ => false
    }
  }

}

class Cache[K, V] extends {
  val data = Ref[Map[K, V]](Map())
  def findOrCreate(k: K, v: V) = {
    val f = (m: Map[K, V]) => m + (k -> v)
    //    data.transform(f);
  }
}

trait NinoToCis {
  def ninoToCis(nino: String) =
    try {
      val full = s"Cis/${nino}.txt"
      val url = getClass.getClassLoader.getResource(full)
      if (url == null)
        <NoCis/>
      else {
        val xmlString = scala.io.Source.fromURL(url).mkString
        val xml = XML.loadString(xmlString)
        xml
      }
    } catch {
      case e: Exception => throw new RuntimeException("Cannot load " + nino, e)
    }
}
object Dbase {
  val dataSource = new BasicDataSource();

  dataSource.setDriverClassName("com.mysql.jdbc.Driver");
  dataSource.setUsername("root");
  dataSource.setPassword("iwtbde");
  dataSource.setUrl("jdbc:mysql://localhost/ca");
  dataSource.setMaxActive(10);
  dataSource.setMaxIdle(5);
  dataSource.setInitialSize(5);

  val template = new JdbcTemplate(dataSource)

}
//trait NinoToDecision {
//  def ninoToDecision(implicit i: InTxn, w: World, nino: String): Elem
//}
//
//class NinoToDecisionMysql() extends NinoToDecision {
//  def addToCache(implicit i: InTxn, w: World, nino: String, result: Elem) = {
//    w.caches.ninoToDecision.transform((m) => m + (nino -> result))
//    result
//  }
//
//  def apply(implicit i: InTxn, w: World, nino: String): Elem = ninoToDecision(i, w, nino)
//
//  def ninoToDecision(implicit i: InTxn, w: World, nino: String): Elem = {
//    val l = Dbase.template.queryForList("SELECT *FROM   INFORMATION_SCHEMA.SYSTEM_TABLES")
//    l.size match {
//      case 0 => addToCache(i, w, nino, <noDecision/>)
//      case 1 => addToCache(i, w, nino, XML.loadString(l.get(0).toString))
//      case _ => throw new IllegalStateException("Have two decisions for nino " + nino);
//    }
//  }
//}
//case class WorldTime(today: DateTime, dateOfClaim: DateTime)
//case class WorldServices(ninoToCis: NinoToCis)
//
//case class World2(time: WorldTime, application: Elem, services: WorldServices) {
//  lazy val nino = (application \\ "ClaimantNINO").text
//  lazy val claimCis = services.ninoToCis.ninoToCis(nino)
//
//  lazy val dpNino = (application \\ "DependantNINO").text
//  lazy val dpCis = services.ninoToCis.ninoToCis(dpNino)
//}

object World {
  implicit def worldToCis(w: World) = w.ninoToCis
  def apply(): World = apply("2010-1-1")
  def apply(claimDate: String): World = apply(Xmls.asDate(claimDate))
  def apply(claimDate: DateTime): World = World(Xmls.asDate("2010-7-5"), claimDate, new NinoToCis() {})

}
case class World(today: DateTime, dateOfClaim: DateTime, ninoToCis: NinoToCis) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor): String =
    "World"
}

case class KeyAndParams(reason: String, params: Any*)

case class ReasonAndAmount(amount: Option[Double], reason: KeyAndParams)
case class ReasonsAndAmount(amount: Option[Double], reasons: List[KeyAndParams])

object ReasonAndAmount {
  def apply(amount: Double, reason: String, params: Any*): ReasonAndAmount = ReasonAndAmount(Some(amount), KeyAndParams(reason, params: _*))
  def apply(reason: String, params: Any*): ReasonAndAmount = ReasonAndAmount(None, KeyAndParams(reason, params: _*))
}

object ReasonsAndValidExpense {
  def combine(reasonAndValidExpenses: ReasonAndAmount*): ReasonsAndAmount = {
    reasonAndValidExpenses.foldLeft(ReasonsAndAmount(None, List()))((acc, re) => {
      if (acc.amount.isDefined || re.amount.isDefined) {
        val l: Double = acc.amount.getOrElse(0.0)
        val r: Double = re.amount.getOrElse(0.0)
        val sum = l + r
        ReasonsAndAmount(Some(sum), re.reason :: acc.reasons)
      } else
        ReasonsAndAmount(None, re.reason :: acc.reasons)
    })
  }
}
