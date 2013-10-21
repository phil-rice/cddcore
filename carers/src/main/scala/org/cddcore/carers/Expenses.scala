package org.cddcore.carers

import org.cddcore.engine.Engine
import scala.xml.Elem
import org.junit.runner.RunWith
import org.cddcore.engine.tests.CddJunitRunner

@RunWith(classOf[CddJunitRunner])
object Expenses {

  val childCareExpenses = Engine[World, Elem, ReasonAndAmount]().
    useCase("ExpensesChild no").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>no</ExpensesPsnPension>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesChildRelationName>
                          <PersonFamilyName>SMITHSON</PersonFamilyName>
                        </ExpensesChildRelationName>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount("expense.child.noExpenses")).
    useCase("ExpensesChild yes ").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>no</ExpensesPsnPension>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>yes</ExpensesChild>
                        <ExpensesChildAmount>30</ExpensesChildAmount>
                        <ExpensesChildRelationSelf>Friend</ExpensesChildRelationSelf>
                        <ExpensesChildRelationName>
                          <PersonFamilyName>SMITHSON</PersonFamilyName>
                        </ExpensesChildRelationName>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount(15, "expense.child.valid")).
    because((w: World, x: Elem) => Xmls.asYesNo((x \\ "ExpensesChild").text)).
    code((w: World, x: Elem) => ReasonAndAmount(Xmls.asFloat((x \\ "ExpensesChildAmount").text) / 2, "expense.child.valid")).
    build

  val privatePensionExpenses = Engine[World, Elem, ReasonAndAmount]().
    useCase("ExpensesPsnPension no").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>no</ExpensesPsnPension>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesChildRelationName>
                          <PersonFamilyName>SMITHSON</PersonFamilyName>
                        </ExpensesChildRelationName>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount("expense.pension.private.noExpenses")).

    useCase("ExpensesPsnPension yes").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>yes</ExpensesPsnPension>
                        <ExpensesPsnPensionAmount>30</ExpensesPsnPensionAmount>
                        <ExpensesPsnPensionPeriodicity>Weekly</ExpensesPsnPensionPeriodicity>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount(15, "expense.pension.private.valid")).
    because((w: World, x: Elem) => Xmls.asYesNo((x \\ "ExpensesPsnPension").text)).
    code((w: World, x: Elem) => ReasonAndAmount(Xmls.asFloat((x \\ "ExpensesPsnPensionAmount").text) / 2, "expense.pension.private.valid")).

    build

  val occupationalExpenses = Engine[World, Elem, ReasonAndAmount]().
    useCase("ExpensesOccPension no").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>no</ExpensesPsnPension>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesChildRelationName>
                          <PersonFamilyName>SMITHSON</PersonFamilyName>
                        </ExpensesChildRelationName>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount("expense.pension.occupational.noExpenses")).
    useCase("ExpensesOccPension yes").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>yes</ExpensesOccPension>
                        <ExpensesOccPensionAmount>30</ExpensesOccPensionAmount>
                        <ExpensesOccPensionPeriodicity>Weekly</ExpensesOccPensionPeriodicity>
                        <ExpensesPsnPension>no</ExpensesPsnPension>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonAndAmount(15, "expense.pension.occupational.valid")).
    because((w: World, x: Elem) => Xmls.asYesNo((x \\ "ExpensesOccPension").text)).
    code((w: World, x: Elem) => ReasonAndAmount(Xmls.asFloat((x \\ "ExpensesOccPensionAmount").text) / 2, "expense.pension.occupational.valid")).

    build

  val expenses = Engine[World, Elem, ReasonsAndAmount]().
    useCase("uc1").
    scenario(World(), <ExpensesData>
                        <ExpensesOccPension>no</ExpensesOccPension>
                        <ExpensesPsnPension>yes</ExpensesPsnPension>
                        <ExpensesPsnPensionAmount>30</ExpensesPsnPensionAmount>
                        <ExpensesPsnPensionPeriodicity>Weekly</ExpensesPsnPensionPeriodicity>
                        <ExpensesNecessary>no</ExpensesNecessary>
                        <ExpensesChild>no</ExpensesChild>
                        <ExpensesCareDP>no</ExpensesCareDP>
                      </ExpensesData>).
    expected(ReasonsAndAmount(Some(15), List(KeyAndParams("expense.pension.occupational.noExpenses"), KeyAndParams("expense.pension.private.valid"), KeyAndParams("expense.child.noExpenses")))).
    code((w: World, x: Elem) => ReasonsAndValidExpense.combine(
      childCareExpenses(w, x),
      privatePensionExpenses(w, x),
      occupationalExpenses(w, x))).
    build

  //    build;
}