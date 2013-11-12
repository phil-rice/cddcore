package org.cddcore.example.tennisScore
import javax.ws.rs._
import org.corecdd.website._
import org.cddcore.engine.Engine
import org.cddcore.engine.Engine2

class TennisPathHandler(scorer: Engine2[Int, Int, String]) extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri.equalsIgnoreCase("/")
  override def paramsINeed(context: HandlerContext) = List("leftScore", "rightScore", "score")
  def leftScore(params: List[(String, String)]) = getParam(params, "leftScore").toInt
  def rightScore(params: List[(String, String)]) = getParam(params, "rightScore").toInt
  def html(context: HandlerContext, params: List[(String, String)]): String = context.method match {
    case "GET" => html(context, 0, 0);
    case "POST" => getParam(params, "score") match {
      case "Left" => html(context, leftScore(params) + 1, rightScore(params))
      case "Right" => html(context, leftScore(params), rightScore(params) + 1)
    }
    case _ => ""
  }
  def html(context: HandlerContext, leftScore: Int, rightScore: Int) = {
    import context._
    val action = fullUri
    val requirements = urlMap.toUrl(scorer)
    <html>
      <body>
        <h1>Tennis Game</h1>
        <a href={ requirements }> Requirements </a>
        <p>Score:{ scorer(leftScore, rightScore) }</p>
        <form method='post' action={ action }>
          <input type='hidden' name='leftScore' value={ leftScore.toString }/>
          <input type='hidden' name='rightScore' value={ rightScore.toString }/>
          <input type='submit' name='score' value='Left'/>
          <input type='submit' name='score' value='Right'/>
        </form>
      </body>
    </html>.toString
  }
}
