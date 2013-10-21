package org.cddcore.carers

import org.junit.runner.RunWith
import org.cddcore.engine.tests.CddJunitRunner
import org.cddcore.engine.Engine
import scala.xml.Elem

@RunWith(classOf[CddJunitRunner])
object Income {

  val income = Engine[World, Elem, ReasonAndAmount]().
    useCase("No employmentData").
    scenario(World(), <root/>).
    expected(ReasonAndAmount("income.notEmployed")).

    useCase("Annually paid").
    scenario(World(), <root><EmploymentData>
                              <EmploymentStartDate>1999-05-09</EmploymentStartDate>
                              <EmploymentJobType>IT SPECIALIST</EmploymentJobType>
                              <EmploymentPayrollNumber>796451234531</EmploymentPayrollNumber>
                              <EmploymentEmployer>S109-8762</EmploymentEmployer>
                              <EmploymentLastPaidDate>2010-01-29</EmploymentLastPaidDate>
                              <EmploymentLastPaidPeriodStart>2010-01-01</EmploymentLastPaidPeriodStart>
                              <EmploymentLastPaidPeriodEnd>2010-01-29</EmploymentLastPaidPeriodEnd>
                              <EmploymentGrossSalary>7000.00</EmploymentGrossSalary>
                              <EmploymentIncludedComments>STUFF...</EmploymentIncludedComments>
                              <EmploymentPayPeriodicity>Annually</EmploymentPayPeriodicity>
                              <EmploymentPayDate>Every Friday</EmploymentPayDate>
                              <EmploymentPaidSameAmount>yes</EmploymentPaidSameAmount>
                              <EmploymentHolidayOrSSP>yes</EmploymentHolidayOrSSP>
                              <EmploymentHourPerWeek>17</EmploymentHourPerWeek>
                              <EmploymentPaidExtras>no</EmploymentPaidExtras>
                              <EmploymentExtrasComment>n/a</EmploymentExtrasComment>
                              <EmploymentOwedMonies>no</EmploymentOwedMonies>
                            </EmploymentData></root>).
    expected(ReasonAndAmount(7000.0 / 52, "income.annual")).
    code((w: World, e: Elem) => ReasonAndAmount(Xmls.asFloat((e \\ "EmploymentGrossSalary").text) / 52, "income.annual")).
    because((w: World, e: Elem) => Xmls.tagPresent(e, "EmploymentData") & "Annually" == ((e \\ "EmploymentPayPeriodicity").text)).

    useCase("Weekly paid").
    scenario(World(), <root><EmploymentData>
                              <EmploymentStartDate>1999-05-09</EmploymentStartDate>
                              <EmploymentJobType>IT SPECIALIST</EmploymentJobType>
                              <EmploymentPayrollNumber>796451234531</EmploymentPayrollNumber>
                              <EmploymentEmployer>S109-8762</EmploymentEmployer>
                              <EmploymentLastPaidDate>2010-01-29</EmploymentLastPaidDate>
                              <EmploymentLastPaidPeriodStart>2010-01-01</EmploymentLastPaidPeriodStart>
                              <EmploymentLastPaidPeriodEnd>2010-01-29</EmploymentLastPaidPeriodEnd>
                              <EmploymentGrossSalary>100.00</EmploymentGrossSalary>
                              <EmploymentIncludedComments>STUFF...</EmploymentIncludedComments>
                              <EmploymentPayPeriodicity>Weekly</EmploymentPayPeriodicity>
                              <EmploymentPayDate>Every Friday</EmploymentPayDate>
                              <EmploymentPaidSameAmount>yes</EmploymentPaidSameAmount>
                              <EmploymentHolidayOrSSP>yes</EmploymentHolidayOrSSP>
                              <EmploymentHourPerWeek>17</EmploymentHourPerWeek>
                              <EmploymentPaidExtras>no</EmploymentPaidExtras>
                              <EmploymentExtrasComment>n/a</EmploymentExtrasComment>
                              <EmploymentOwedMonies>no</EmploymentOwedMonies>
                            </EmploymentData></root>).
    expected(ReasonAndAmount(100.0, "income.weekly")).
    code((w: World, e: Elem) => ReasonAndAmount(Xmls.asFloat((e \\ "EmploymentGrossSalary").text), "income.weekly")).
    because((w: World, e: Elem) => Xmls.tagPresent(e, "EmploymentData") & "Weekly" == ((e \\ "EmploymentPayPeriodicity").text)).
    build

  //  val income = Engine[World, Elem, ReasonsAndAmount]().
  //    useCase("Simple weekly paid person").
  //    scenario(World.blankTestWorld, <root><EmploymentData>
  <EmploymentGrossSalary>100.00</EmploymentGrossSalary>
  <EmploymentPayPeriodicity>Weekly</EmploymentPayPeriodicity>
  //                                         </EmploymentData></root>).
  //    expected(ReasonsAndAmount(Some(100), List(KeyAndParams("income.weekly"), KeyAndParams("expense.pension.private.valid"), KeyAndParams("expense.child.noExpenses")))).
  //    code((w: World, x: Elem) => ReasonsAndValidExpense.combine(
  //      incomeFromEmploymentData(w, x)
  //      )).
  //    build

}