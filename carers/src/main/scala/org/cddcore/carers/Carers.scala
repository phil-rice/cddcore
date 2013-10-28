package org.cddcore.carers

import scala.language.implicitConversions
import scala.xml.Elem

import org.cddcore.engine._
import org.cddcore.engine.tests.CddJunitRunner
import org.joda.time.DateTime
import org.junit.runner.RunWith

case class CarersXmlSituation(w: World, e: Elem) extends XmlSituation {
  import Xml._
  lazy val birthdate = xml(e) \ "ClaimantData" \ "ClaimantBirthDate" \ "PersonBirthDate" \ date
  lazy val DependantNino = xml(e) \ "DependantData" \ "DependantNINO" \ string
  lazy val ClaimAlwaysUK = xml(e) \ "ClaimData" \ "ClaimAlwaysUK" \ yesNo(default = false)
  lazy val Claim35Hours = xml(e) \ "ClaimData" \ "Claim35Hours" \ yesNo(default = false)
  lazy val ClaimCurrentResidentUK = xml(e) \ "ClaimData" \ "ClaimCurrentResidentUK" \ yesNo(default = false)
  lazy val ClaimEducationFullTime = xml(e) \ "ClaimData" \ "ClaimEducationFullTime" \ yesNo(default = false)
  lazy val ClaimRentalIncome = xml(e) \ "ClaimData" \ "ClaimRentalIncome" \ yesNo(default = false)
  lazy val ClaimStartDate = xml(e) \ "ClaimData" \ "ClaimStartDate" \ date

  lazy val claimBreaksFrom = xml(e) \ "ClaimData" \ "ClaimBreaks" \ "BreakInCare" \ "BICFromDate" \ date \ list[DateTime]
  lazy val claimBreaksTo = xml(e) \ "ClaimData" \ "ClaimBreaks" \ "BreakInCare" \ "BICToDate" \ date \ list[DateTime]

  lazy val dependantXml: Elem = DependantNino.get() match {
    case Some(s) => w.ninoToCis(s);
    case None => <NoDependantXml/>
  }
  lazy val DependantAwardComponent = xml(dependantXml) \\ "AwardComponent" \ string

  lazy val expenses = Expenses.expenses(w, e)
  lazy val income = Income.income(w, e)

  //  lazy val howLongHasClaimBeenActiveInWeeks = 

  lazy val nettIncome: Option[Double] =
    for (e <- expenses.amount; i <- income.amount)
      yield i - e

  lazy val incomeOk =
    nettIncome match {
      case Some(i) => i < 100
      case _ => false
    }
  override def toString = getClass.getSimpleName() + s"(\n  expenses=${expenses}\n  income=${income}\n  nettIncome=${nettIncome}\n  incomeOk = ${incomeOk}\n  ${fragmentsToString}\n${xmlsToString})"

}

@RunWith(classOf[CddJunitRunner])
object Carers {
  implicit def worldElemToCarers(x: Tuple2[World, Elem]) = CarersXmlSituation(x._1, x._2)
  implicit def worldStringToCarers(x: Tuple2[World, String]) = CarersXmlSituation(x._1, Xmls.validateClaim(x._2))
  //  implicit def carersToWorld(x: CarersXmlSituation) = x.w
  //  implicit def carersToElem(x: CarersXmlSituation) = x.e

  val engine = Engine[CarersXmlSituation, ReasonAndAmount]().
    code((c: CarersXmlSituation) => ReasonAndAmount("carer.default.notPaid")).
    useCase("Customers under age 16 are not entitled to CA").
    scenario((World("2010-6-9"), "CL100104A"), "Cl100104A-Age Under 16").
    expected(ReasonAndAmount("carer.claimant.under16")).
    because((c: CarersXmlSituation) => c.birthdate.get() match {
      case Some(bd) => bd.plusYears(16).isAfter(c.w.today)
      case _ => false
    }).

    useCase("Hours1 - Customers with Hours of caring must be 35 hours or more in any one week").
    scenario((World("2010-1-1"), "CL100105A"), "CL100105A-lessThen35Hours").
    expected(ReasonAndAmount("carer.claimant.under35hoursCaring")).
    because((c: CarersXmlSituation) => !c.Claim35Hours()).

    useCase("Qualifying Benefit 3 - DP's without the required level of qualyfing benefit will result in the disallowance of the claim to CA.").
    scenario((World("2010-6-23"), "CL100106A"), "CL100106A-?????? ").
    expected(ReasonAndAmount("carer.qualifyingBenefit.dpWithoutRequiredLevelOfQualifyingBenefit")).
    because((c: CarersXmlSituation) => c.DependantAwardComponent() != "DLA Middle Rate Care").

    useCase("Residence 3- Customer who is not considered resident and present in GB is not entitled to CA.").
    scenario((World("2010-6-7"), "CL100107A"), "CL100107A-notInGB").
    expected(ReasonAndAmount("carers.claimant.notResident")).
    because((c: CarersXmlSituation) => !c.ClaimAlwaysUK()).

    useCase("Presence 2- Customers who have restrictions on their immigration status will be disallowed CA.").
    scenario((World("2010-6-7"), "CL100108A"), "CL100108A-restriction on immigration status").
    expected(ReasonAndAmount("carers.claimant.restriction.immigrationStatus")).
    because((c: CarersXmlSituation) => !c.ClaimCurrentResidentUK()).

    useCase("Full Time Eduction 2  -Customer in FTE 21 hours or more each week are not entitled to CA.").
    scenario((World("2010-2-10"), "CL100109A"), "CL100109A-full time education").
    expected(ReasonAndAmount("carers.claimant.fullTimeEduction.moreThan21Hours")).
    because((c: CarersXmlSituation) => c.ClaimEducationFullTime()).

    useCase("Employment 4  - Customer's claiming CA may claim an allowable expense of up to 50% of their childcare expenses where the child care is not being undertaken by a direct relative. This amount may then be deducted from their gross pay.").
    scenario((World("2010-3-22"), "CL100110A"), "CL100110A-child care allowance").
    expected(ReasonAndAmount("carers.validClaim", Some(95.0))).
    code((c: CarersXmlSituation) => ReasonAndAmount("carers.validClaim", c.nettIncome)).
    because((c: CarersXmlSituation) => c.incomeOk).

    useCase("Employment 5 - Customers claiming CA may claim an allowable expense of up to 50% of their Private Pension contributions. This amount may then be deducted from their gross pay figure.").
    scenario((World("2010-3-8"), "CL100111A"), "CL100111A-private pension").
    expected(ReasonAndAmount("carers.validClaim", Some(95.0))).

    useCase("Employment 6 - Customers claiming CA may claim an allowable expense of up to 50% of their Occupational Pension contributions. This amount may then be deducted from their gross pay figure.").
    scenario((World("2010-3-8"), "CL100112A"), "CL100112A-occupational pension").
    expected(ReasonAndAmount("carers.validClaim", Some(95.0))).

    useCase("Employment 7 - Customer in paid employment exceeding GBP100 (after allowable expenses) per week is not entitled to CA.").
    scenario((World("2010-6-1"), "CL100113A"), "CL100113A-paid employment earning too much").
    expected(ReasonAndAmount("carers.nettIncome.moreThan100PerWeek", None)).
    because((c: CarersXmlSituation) => !c.incomeOk).

    useCase("Self employment 2 - Customer in Self employed work earning more than the prescribed limit of GBP100 per week (after allowable expenses) are not entitled to CA.").
    scenario((World("2010-3-1"), "CL100114A"), "CL114A-self employed earning too much").
    expected(ReasonAndAmount("carers.nettIncome.moreThan100PerWeek", None)).

    useCase("Sublet 2- Customers receiving payment for subletting their property for board and lodgings receiving more than the prescribed limit of GBP100 (after allowable expenses) will be disallowed for CA.").
    scenario((World("2010-3-1"), "CL100115A"), "CL115A-sub let").
    expected(ReasonAndAmount("carers.income.rental", None)).
    because((c: CarersXmlSituation) => c.ClaimRentalIncome()).

    useCase("Prop 2- Customer receiving an Income from the renting of another property or land in the UK or abroad either their own name or a share in a partners profit is above GBP100 per week(after allowable expenses) is not entitled to CA.").
    scenario((World("2010-3-1"), "CL100116A"), "CL116A-income from renting").
    expected(ReasonAndAmount("carers.income.rental", None)).

    //    useCase("Customer receiving certain other benefits at a rate lower than the rate of CA will reduce the payable amount of CA.").
    //    scenario((World("2010-3-1"), "CL100117A"), "CL117A-income from renting - pension from 10/4/2010. Date is before the pension started.").
    //    scenario((World("2010-5-1"), "CL100117A"), "CL117A-income from renting - pension from 10/4/2010. Date is after the pension started.").

    useCase("Customer receiving certain other benefits at a rate lower than the rate of CA will reduce the payable amount of CA.").

    build

  def main(args: Array[String]) {
    val project = Project("Carers", engine, Income.income, Expenses.expenses, Expenses.childCareExpenses, Expenses.occupationalExpenses, Expenses.privatePensionExpenses)
    ReportCreator.fileSystem(project).create
    println("done")

  }

}