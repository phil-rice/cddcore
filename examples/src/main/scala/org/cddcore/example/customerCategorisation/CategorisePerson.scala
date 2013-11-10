package org.cddcore.example.customerCategorisation

import scala.language.implicitConversions
import org.cddcore.engine._
import org.junit.runner.RunWith
import org.cddcore.engine.tests._
import org.cddcore.example.processCheque_DM_1.GBP
import javax.ws.rs._
import org.corecdd.website.WebServer

case class Person(savings: GBP, ageInYears: Int) {
  lazy val hasEnoughSavings = savings >= 1000
  lazy val tooYoung = ageInYears < 16
}

@RunWith(classOf[CddJunitRunner])
object CategorisePerson {
  import GBP._
  val funcSpec = Document(name = Some("Function Spec"), url = Some("http://www.autotdd.com/wiki/Comparison_with_JBoss_/_Drools"))
  val changeRequest = Document(name = Some("CR001"), url = Some("http://www.autotdd.com/wiki/Comparison_with_JBoss_/_Drools"))
  //  useCase("Bribery works for young people").expected("person.reallyRichKid").reference("1.2.7", changeRequest).
  //  scenario(Person(savings=1000000, ageInYears=10)). because((p:Person) => p.savings>100000).

  val categorise = Engine[Person, String]().description("This engine works out what category of customer you are").
      param((s: String)=> s.split(",") match {case Array(savings,age) => Person(Integer.parseInt(savings), Integer.parseInt(age))}, "Savings,Age"). 
    useCase("Young people are not eligible", " A person under 16 cannot use this system").expected("person.child").
    scenario(Person(savings = 10000, ageInYears = 10), "Child aged 10, and loads of money").
    because((p: Person) => p.tooYoung).
    scenario(Person(savings = 100, ageInYears = 15), "Child aged 15").

    useCase("Poor people").expected("person.poor").
    scenario(Person(savings = 50, ageInYears = 20), "Very poor person").
    because((p: Person) => (!p.hasEnoughSavings)).
    scenario(Person(savings = 999, ageInYears = 20), "Only just poor person").

    useCase("Rich people").description("Rich people have lots of savings").expected("person.rich").
    scenario(Person(savings = 1050, ageInYears = 20), "Rich person").
    because((p: Person) => p.hasEnoughSavings).
    build

  def main(args: Array[String]) {
    println(categorise(Person(40, 100)))
  }

}
 

