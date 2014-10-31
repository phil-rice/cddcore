package org.cddcore.example.tennisScore

import org.cddcore.engine.Document
import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.tests.CddJunitRunner

@RunWith(classOf[CddJunitRunner])
object TennisScorer {
  val definition = Document(title = Some("CodingDojo"), url = Some("http://codingdojo.org/cgi-bin/wiki.pl?KataTennis"))
  val wikipedia = Document(title = Some("Wikipedia"), url = Some("http://en.wikipedia.org/wiki/Tennis_score"))
  val changeRequest = Document(title = Some("CR24"), url = Some("http://en.wikipedia.org/wiki/Tennis_score"))

  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  val scorer = Engine[Int, Int, String]().title("Tennis Kata").reference("", wikipedia).reference("", definition).reference("").
    useCase("Winning", "A game is won by the first player to have won at least four points in total and at least two points more than the opponent.").
    reference("2", definition).

    useCase("Server Winning").reference("3.8", changeRequest).
    reference("2.1", definition).

    scenario(4, 0).expected("left won").because((l: Int, r: Int) => (l - r) >= 2 && l >= 4).
    scenario(4, 1).expected("left won").
    scenario(4, 2).expected("left won").
    scenario(5, 3).expected("left won").

    useCase("Receiver winning", "here are some words about that").
    reference("2.1", definition).

    scenario(0, 4).expected("right won").because((l: Int, r: Int) => (r - l) >= 2 && r >= 4).
    scenario(1, 4).expected("right won").
    scenario(2, 4).expected("right won").
    scenario(3, 5).expected("right won").
    scenario(40, 42).expected("right won").

    useCase("Running score", "The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.").
    reference("2.10", definition).
    scenario(2, 3).expected("thirty, forty").because((l: Int, r: Int) => l < 4 && r < 4).code((l: Int, r: Int) => s"${lookup(l)}, ${lookup(r)}").
    scenario(2, 1).expected("thirty, fifteen").

    useCase("When both have the same running score", "The running score, if both scores are the same, is called xx all").
    reference("2.11", definition).
    scenario(0, 0).expected("love all").because((l: Int, r: Int) => l == r && l < 3).code((l: Int, r: Int) => s"${lookup(l)} all").
    scenario(2, 2).expected("thirty all").

    useCase("Deuce").description("If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.").expected("deuce").priority(1).
    reference("5", definition).
    scenario(3, 3).because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r).
    scenario(4, 4).
    scenario(6, 6).

    useCase("Advantage").description("If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.").
    reference("3", definition).
    scenario(5, 4).expected("advantage left").because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r + 1).
    scenario(6, 5).expected("advantage left").reference("2").
    scenario(4, 3).expected("advantage left").

    scenario(4, 5).expected("advantage right").because((l: Int, r: Int) => l >= 3 && r >= 3 && r == l + 1).
    scenario(5, 6).expected("advantage right").

    build

}

