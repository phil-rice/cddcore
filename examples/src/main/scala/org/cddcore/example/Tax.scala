package org.cddcore.example

import java.util.Date
import org.cddcore.engine.Engine
import java.text.SimpleDateFormat
import org.junit.runner.RunWith
import org.cddcore.engine.tests._

case class Person(name: String, income: Float, dateOfBirth: Date)

case class AllowanceAndLimit(allowance: Float, limit: Float)
case class SinglePersonsAllowanceReference(basicAllowance: Float, after5thApril1948: AllowanceAndLimit, after6thApril1938: AllowanceAndLimit, before6thApril1938: AllowanceAndLimit)

object Dates {
  val dateFormatter = new SimpleDateFormat("dd/MM/yyyy")
  val april1st1930 = dateFormatter.parse("01/04/1930")
  val april1st1940 = dateFormatter.parse("01/04/1940")
  val april1st1960 = dateFormatter.parse("01/04/1960")

  val april6th1938 = dateFormatter.parse("06/04/1938")
  val april6th1948 = dateFormatter.parse("06/04/1948")

}

object TaxReference {
  val singlePersonsAllowanceReference = SinglePersonsAllowanceReference(
    basicAllowance = 9440,
    after5thApril1948 = AllowanceAndLimit(9440, 100000),
    after6thApril1938 = AllowanceAndLimit(10500, 26100),
    before6thApril1938 = AllowanceAndLimit(10660, 26100))

}

class Tax {

}
@RunWith(classOf[CddJunitRunner])
object Tax {

  val allowanceSelectionByDate = Engine[Date, SinglePersonsAllowanceReference, AllowanceAndLimit]().
    useCase("Born before 6th April 1938").
      scenario(Dates.april1st1930, TaxReference.singlePersonsAllowanceReference, "Few years before").
         expected(AllowanceAndLimit(10660, 26100)).
         code((d: Date, ref: SinglePersonsAllowanceReference) => ref.before6thApril1938).

    useCase("Born between 6 April 1938 and 5 April 1948").
       scenario(Dates.april1st1940, TaxReference.singlePersonsAllowanceReference, "In the middle").
          expected(AllowanceAndLimit(10500, 26100)).
          code((d: Date, ref: SinglePersonsAllowanceReference) => ref.after6thApril1938).
          because((d: Date, ref: SinglePersonsAllowanceReference) => d.after(Dates.april6th1938)).
    
    useCase("Born after 5 April 1948").
      scenario(Dates.april1st1960, TaxReference.singlePersonsAllowanceReference, "Some time after").
         expected(AllowanceAndLimit(9440, 100000)).
             code((d: Date, ref: SinglePersonsAllowanceReference) => ref.after5thApril1948).
             because((d: Date, ref: SinglePersonsAllowanceReference) => d.after(Dates.april6th1948)).

    build

  val reducePersonalAllowanceByIncome = Engine[Float, AllowanceAndLimit, Float, Float]().
    useCase("Higher Personal Allowance Not Reduced ").
    scenario(15000, AllowanceAndLimit(9440, 100000), 9400, "Edward").
    expected(15000).
    code((income: Float, a: AllowanceAndLimit, basicAllowance: Float) => {
      val excess = Math.max(0, income - a.limit)
      val reduction = excess / 2
      val result = Math.max(basicAllowance, a.allowance - reduction)
      result
    }).

    useCase("Higher Personal Allowance Partially Reduced ").
    scenario(28000, AllowanceAndLimit(10660, 26100), 9440, "Diana").
    expected(9710).

    useCase("Higher Personal Allowance Reduced To Basic").
    scenario(28000, AllowanceAndLimit(10660, 26100), 9440, "Charles").
    build

  //This just ties it all together. 
  val singlePersonsAllowance = Engine[Person, SinglePersonsAllowanceReference, Float].
    useCase("Personal Allowance - If you were born before 6 April 1948 and your income is between26,100 and 100,000"). //http://www.google.co.uk 
    scenario(Person("Edward", 20000, Dates.april1st1930), TaxReference.singlePersonsAllowanceReference, "Edward  born before 6 April 1938, higher Personal Allowance not reduced").
    expected(10660).
    code((p: Person, a: SinglePersonsAllowanceReference) => reducePersonalAllowanceByIncome(p.income, allowanceSelectionByDate(p.dateOfBirth, a), a.basicAllowance)).

    scenario(Person("Diana", 28000, Dates.april1st1930), TaxReference.singlePersonsAllowanceReference, "Diana  born before 6 April 1938, higher Personal Allowance partially reduced ").
    expected(9710).
    because((p: Person, a: SinglePersonsAllowanceReference) => p.dateOfBirth.before(Dates.april6th1938) & p.income > a.before6thApril1938.limit).

    scenario(Person("Charles", 40000, Dates.april1st1940), TaxReference.singlePersonsAllowanceReference, "Charles  born between 6 April 1938 and 5 April 1948, higher Personal Allowance reduced to basic Personal Allowance").
    build

}