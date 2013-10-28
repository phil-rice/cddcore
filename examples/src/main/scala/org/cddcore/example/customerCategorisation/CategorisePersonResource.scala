package org.cddcore.example.customerCategorisation

import javax.ws.rs.Consumes
import javax.ws.rs.FormParam
import javax.ws.rs.GET
import javax.ws.rs.POST
import javax.ws.rs.Path
import org.cddcore.example.processCheque_DM_1.GBP

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