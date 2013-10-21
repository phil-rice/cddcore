package org.cddcore.example.customerCategorisation

import scala.language.implicitConversions
import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.engine.tests._
import org.cddcore.example.processCheque_DM_1.GBP
import javax.ws.rs._

case class Person(savings: GBP, ageInYears: Int) {
  lazy val hasEnoughSavings = savings >= 1000
  lazy val tooYoung = ageInYears < 16
}

@RunWith(classOf[CddJunitRunner])
object CategorisePerson {
  import GBP._
  val categorise = Engine[Person, String]().
    description("This engine works out what category of customer you are").
    code((p: Person) => throw new IllegalStateException).

    useCase("Young people are not eligable").expected("person.child").
    scenario(Person(savings = 10000, ageInYears = 10), "Child aged 10").
    because((p: Person) => p.tooYoung).

    scenario(Person(savings = 10000, ageInYears = 15), "Child aged 15").

    useCase("Poor people").expected("person.poor").
    scenario(Person(savings = 50, ageInYears = 20), "Very poor person").
    because((p: Person) => (!p.hasEnoughSavings)).

    scenario(Person(savings = 999, ageInYears = 20), "Only just poor person").

    useCase("Rich people").expected("person.rich").
    scenario(Person(savings = 1050, ageInYears = 20), "Rich person").
    because((p: Person) => p.hasEnoughSavings).

    build

  def main(args: Array[String]) {
    org.cddcore.example.WebServer.launch("org.cddcore.example.customerCategorisation")
    println(categorise(Person(40, 100)))
    println(categorise(Person(1040, 100)))
  }

}

object CategorisePersonResource {
  final val path = "/person"
  final val formUrlEncoded = "application/x-www-form-urlencoded"
}

@Path(CategorisePersonResource.path)
class CategorisePersonResource() {
  import GBP._

  @GET
  def start() = html(0, 0)

  @POST @Consumes(Array(CategorisePersonResource.formUrlEncoded))
  def continue(@FormParam("savings") savings: Int, @FormParam("age") age: Int) = {
    html(savings, age)
  }

  def html(savings: Int, age: Int) =
    <html>
      <body>
        <h1>Person Categorisation</h1>
        <form method='post' action={ CategorisePersonResource.path }>
          <table>
            <tr>
              <td><label name='age'>Age</label></td>
              <td><input type='text' name='age' value={ age.toString }/></td>
            </tr>
            <tr>
              <td><label name='savings'>Savings</label></td>
              <td><input type='text' name='savings' value={ savings.toString }/></td>
            </tr>
          </table>
          <input type='submit'/>
        </form>
        <p>Category:&nbsp;{ CategorisePerson.categorise(Person(savings, age)) }</p>
      </body>
    </html>.toString

}