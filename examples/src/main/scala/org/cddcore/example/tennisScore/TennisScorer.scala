package org.cddcore.example.tennisScore

import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.engine.tests._

@RunWith(classOf[CddJunitRunner])
object TennisScorer {
  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  val scorer = Engine[Int, Int, String]().
   description("Tennis Kata specified by http://codingdojo.org/cgi-bin/wiki.pl?KataTennis").
    withDefaultCode((l: Int, r: Int) => "error").
    useCase("A game is won by the first player to have won at least four points in total and at least two points more than the opponent.").
    scenario(4, 0).expected("left won").because((l: Int, r: Int) => (l - r) >= 2 && l >= 4).
    scenario(4, 1).expected("left won").
    scenario(4, 2).expected("left won").
    scenario(5, 3).expected("left won").

    scenario(0, 4).expected("left won").because((l: Int, r: Int) => (r - l) >= 2 && r >= 4).
    scenario(1, 4).expected("left won").
    scenario(2, 4).expected("left won").
    scenario(3, 5).expected("left won").
    scenario(40, 42).expected("left won").

    useCase("The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.").
    scenario(2, 3).expected("thirty, forty").because((l: Int, r: Int) => l < 4 && r < 4).code((l: Int, r: Int) => s"${lookup(l)}, ${lookup(r)}").
    scenario(2, 1).expected("thirty, fifteen").

    useCase("The running score, if both scores are the same, is called xx all").
    scenario(0, 0).expected("love all").because((l: Int, r: Int) => l == r && l < 3).code((l: Int, r: Int) => s"${lookup(l)} all").
    scenario(2, 2).expected("thirty all").

    useCase("If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.").
    scenario(3, 3).expected("deuce").because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r).priority(1).
    scenario(4, 4).expected("deuce").
    scenario(6, 6).expected("deuce").

    useCase("If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.").
    scenario(5, 4).expected("advantage left").because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r + 1).
    scenario(6, 5).expected("advantage left").
    scenario(4, 3).expected("advantage left").

    scenario(4, 5).expected("advantage right").because((l: Int, r: Int) => l >= 3 && r >= 3 && r == l + 1).
    scenario(5, 6).expected("advantage right").
    scenario(3, 4).expected("advantage right").

    build
}

