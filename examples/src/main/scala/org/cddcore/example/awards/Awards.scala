package org.cddcore.example.awards

import scala.language.implicitConversions
import org.cddcore.engine._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.junit.runner.RunWith
import org.cddcore.tests.CddJunitRunner

case class Award(code: String, comment: String, claimStatus: String, awardStartDate: DateTime)

@RunWith(classOf[CddJunitRunner])
object Award {
  private val formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
  implicit def asDate(s: String): DateTime = formatter.parseDateTime(s);
  implicit def toAward(x: (String, String, String, String)) = Award(x._1, x._2, x._3, asDate(x._4))
  //  implicit def toDate(x: String) = Claim.asDate(x)
  val checkQualifyingBenefit = Engine[DateTime, Award, Boolean]().title("Check for qualifying Benefit").

    useCase("QB not in payment or future dated").
    scenario("2010-05-28", ("AA", "AA lower rate", "Active", "2010-05-27"), "QB start date in past").expected(true).
    because((d: DateTime, a: Award) => d.isAfter(a.awardStartDate)).

    scenario("2010-05-28", ("AA", "AA lower rate", "Active", "2010-05-28"), "QB start date exact").expected(true).
    because((d: DateTime, a: Award) => d == a.awardStartDate).

    scenario("2010-05-28", ("AA", "AA lower rate", "Active", "2010-05-29"), "QB start date in future").expected(false).
    because((d: DateTime, a: Award) => d.isBefore(a.awardStartDate)).

    scenario("2010-05-28", ("AA", "AA lower rate", "Inactive", "2010-06-01"), "QB not in payment").expected(false).
    because((d: DateTime, a: Award) => a.claimStatus != "Active").
    build

  def main(args: Array[String]) {

  }
}