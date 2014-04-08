package org.cddcore.example.folding

import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.tests.CddJunitRunner
import org.cddcore.engine.Reportable

case class Person(hasPet: Boolean = false, hasWife: Boolean = false, isRich: Boolean = false, isInTrouble: Boolean = false, hasWorkIssues: Boolean = false)
/**
 * This is just an example of folding. There are a number of minor engines, each working out if you are 'happy; for 'this reason'.
 *  The person is only happy is they have all the reasons to be happy.
 */
@RunWith(classOf[CddJunitRunner])
object Happy {
  val engine = Engine.folding[Person, Boolean, Boolean]((acc: Boolean, h: Boolean) => acc && h, true).
    title("Happy Engine").
    childEngine("Happy people have pets").
    scenario(Person(hasPet = false)).expected(false).
    scenario(Person(hasPet = true)).expected(true).because((p: Person) => p.hasPet).
    childEngine("Happy people have wives").
    scenario(Person(hasWife = false)).expected(false).
    scenario(Person(hasWife = true)).expected(true).because((p: Person) => p.hasWife).
    childEngine("Happy people are rich").
    scenario(Person(isRich = false)).expected(false).
    scenario(Person(isRich = true)).expected(true).because((p: Person) => p.isRich).
    childEngine("Happy people are not in trouble").
    scenario(Person(isInTrouble = true)).expected(false).
    scenario(Person(isInTrouble = false)).expected(true).because((p: Person) => !p.isInTrouble).
    childEngine("Happy people don't have work issues").
    scenario(Person(hasWorkIssues = true)).expected(false).
    scenario(Person(hasWorkIssues = false)).expected(true).because((p: Person) => !p.hasWorkIssues).
    build;

  def main(args: Array[String]) {
    println("Template: " + Reportable.templateName(engine))
    println(engine)
  }
}