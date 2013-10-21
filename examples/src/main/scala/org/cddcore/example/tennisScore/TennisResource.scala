package org.cddcore.example.tennisScore
import javax.ws.rs._

object TennisResource {
  final val path = "/tennis"
  final val formUrlEncoded = "application/x-www-form-urlencoded"
}

@Path(TennisResource.path)
class TennisResource() {

  @GET
  def start() = html(0, 0)

  @POST @Consumes(Array(TennisResource.formUrlEncoded))
  def continue(@FormParam("leftScore") leftScore: Int, @FormParam("rightScore") rightScore: Int, @FormParam("score") score: String) = {
    score match {
      case "Left" => html(leftScore + 1, rightScore)
      case "Right" => html(leftScore, rightScore + 1)
    }
    //    println(s"Left: ${leftScore}/${score} Right ${rightScore}")
  }

  def html(leftScore: Int, rightScore: Int) =
    <html>
      <body>
        <h1>Tennis Game</h1>
        <p>Score:{ TennisScorer.scorer(leftScore, rightScore) }</p>
        <form method='post' action={ TennisResource.path }>
          <input type='hidden' name='leftScore' value={ leftScore.toString }/>
          <input type='hidden' name='rightScore' value={ rightScore.toString }/>
          <input type='submit' name='score' value='Left'/>
          <input type='submit' name='score' value='Right'/>
        </form>
      </body>
    </html>.toString

}