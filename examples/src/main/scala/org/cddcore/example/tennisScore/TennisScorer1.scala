package org.cddcore.example.tennisScore

import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.engine.tests.CddJunitRunner

object Score {
  val love = Score("love")
  val s15 = Score("fifteen")
  val s30 = Score("thirty")
  val s40 = Score("forty")
  val deuce = Score("deuce")
  val advantage = Score("advantage")
  val noScore = Score("noScore")
  val error = Score("error")
  val won = Score("won")
  val lost = Score("lost")
}
case class Score(item: String)
@RunWith(classOf[CddJunitRunner])
object TennisScorer1 {
  import Score._
  val lookup = Map(0 -> love, 1 -> s15, 2 -> s30, 3 -> s40)

  val scorer = Engine[Int, Int, (Score, Score)]().
    description("Tennis Kata specified by http://codingdojo.org/cgi-bin/wiki.pl?KataTennis").
    code((l: Int, r: Int) => throw new IllegalStateException).
    useCase("A game is won by the first player to have won at least four points in total and at least two points more than the opponent.").
    scenario(4, 0).expected((won, lost)).because((l: Int, r: Int) => (l - r) >= 2 && l >= 4).
    scenario(4, 1).expected((won, lost)).
    scenario(4, 2).expected((won, lost)).
    scenario(5, 3).expected((won, lost)).

    scenario(0, 4).expected((lost, won)).because((l: Int, r: Int) => (r - l) >= 2 && r >= 4).
    scenario(1, 4).expected((lost, won)).
    scenario(2, 4).expected((lost, won)).
    scenario(3, 5).expected((lost, won)).

    useCase("The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.").
    scenario(0, 0).expected((love, love)).
    scenario(2, 3).expected((s30, s40)).
    scenario(2, 1).expected((s30, s15)).
    scenario(3, 3).expected((s40, s40)).
    scenario(0, 0).expected((love, love)).because((l: Int, r: Int) => l < 4 && r < 4).code((l: Int, r: Int) => (lookup(l), lookup(r))).
    scenario(2, 3).expected((s30, s40)).
    scenario(2, 1).expected((s30, s15)).
    scenario(3, 3).expected((s40, s40)).

    useCase("If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.").
    scenario(4, 4).expected((deuce, deuce)).because((l: Int, r: Int) => l > 3 && r > 3 && l == r).
    scenario(6, 6).expected((deuce, deuce)).

    useCase("If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.").
    scenario(5, 4).expected((advantage, noScore)).because((l: Int, r: Int) => l > 3 && r > 3 && l == r + 1).
    scenario(6, 5).expected((advantage, noScore)).
    scenario(4, 5).expected((noScore, advantage)).because((l: Int, r: Int) => l > 3 && r > 3 && r == l + 1).
    scenario(5, 6).expected((noScore, advantage)).
    build
}