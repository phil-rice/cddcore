package org.cddcore.example.legacy

import scala.io.Source
import scala.language.implicitConversions
import org.cddcore.engine.AnySingleConclusionEngine
import org.cddcore.engine.Engine
import org.cddcore.htmlRendering.Report
import org.cddcore.legacy.Legacy
import org.cddcore.legacy.LegacyData
import org.cddcore.legacy.LegacyReport
import org.cddcore.legacy.LegacyReportRenderer
import org.cddcore.legacy.MemoryLegacyMonitor
import org.cddcore.legacy.SingleConclusionLegacy
import org.cddcore.utilities.Files
import java.io.File

object TennisExample {

  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  val tennis = Engine[Int, Int, String]().title("Tennis scorer").
    useCase("Winning", "A game is won by the first player to have won at least four points in total and at least two points more than the opponent.").
    //    scenario(4, 0).expected("left won").
    //    because((l: Int, r: Int) => (l - r) >= 2 && l >= 4).
    //    scenario(4, 1).expected("left won").
    //    scenario(4, 2).expected("left won").
    //    scenario(5, 3).expected("left won").

    //    scenario(0, 4).expected("right won").
    //    because((l: Int, r: Int) => (r - l) >= 2 && r >= 4).
    //    scenario(1, 4).expected("right won").
    //    scenario(2, 4).expected("right won").
    //    scenario(3, 5).expected("right won").
    //    scenario(40, 42).expected("right won").

    useCase("Running score").description("The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as 'love', 'fifteen', 'thirty', and 'forty' respectively.").
    //    scenario(2, 3).expected("thirty - forty").
    //    because((l: Int, r: Int) => l < 4 && r < 4).
    //    code((l: Int, r: Int) => lookup(l) + " - " + lookup(r)).
    //    scenario(2, 1).expected("thirty - fifteen").
    //
    useCase("When both have the same running score").description("The running score, if both scores are the same, is called xx all").
    //    scenario(0, 0).expected("love all").
    //    code((l: Int, r: Int) => lookup(l) + " all").
    //    because((l: Int, r: Int) => l == r).
    //    scenario(2, 2).expected("thirty all").
    //
    //    useCase("Deuce").description("If at least three points have been scored by each player, and the scores are equal, the score is 'deuce'.").expected("deuce").
    //    scenario(3, 3).because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r).
    //    priority(1).
    //    scenario(4, 4).
    //    scenario(6, 6).
    //
    useCase("Advantage").description("If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is 'advantage' for the player in the lead.").
    //    scenario(5, 4).expected("advantage left").because((l: Int, r: Int) => l >= 3 && r >= 3 && l == r + 1).
    //    scenario(6, 5).expected("advantage left").
    //    scenario(4, 3).expected("advantage left").
    //
    //    scenario(4, 5).expected("advantage right").because((l: Int, r: Int) => l >= 3 && r >= 3 && r == l + 1).
    //    scenario(5, 6).expected("advantage right").
    //    scenario(3, 4).expected("advantage right").
    build

  implicit def toRight(x: String) = Right(x)
  val categoriserEngine = Engine[LegacyData[Int, (Int, Int), String], String]().title("Categorise Results").
    code((l) => "Fail").
    //    useCase("Identical").
    //    scenario(LegacyData(1, List(1, 1), "X", "Y")).expected("Identical").because((l: LegacyData[Int, String]) => l.fail && l.p0 == l.p1).

    useCase("Pass").expected("Pass").
    scenario(LegacyData(1, (1, 2), None, "X", "X")).because((l) => l.pass).
    code((l) => "Pass").
    build

  val legacyData = Source.fromFile("TennisLegacyData.dat").mkString
  val lines = legacyData.split("\n").zipWithIndex
  val idToParams = lines.foldLeft(Map[Int, (Int, Int)]())((map, tuple) => tuple match {
    case (line, index) => map + (index.toInt -> (line.substring(0, 1).toInt, line.substring(2, 3).toInt))
  })
  val idToExpected = lines.foldLeft(Map[Int, Either[Exception, String]]())((map, tuple) => tuple match {
    case (line, index) =>
      map + (index.toInt -> line.substring(5).trim)
  })

  val reporter = new MemoryLegacyMonitor[Int, (Int, Int), String]()
  val allIds = 0 to lines.size - 1
  val descFn = (id: Int) =>
    Some(s"scenario(${idToParams(id)}).expected(${"\"" + idToExpected(id) + "\""}).")

  def main(args: Array[String]) {
    val legacy = new SingleConclusionLegacy[Int, (Int, Int), String](allIds,
      idToParams,
      idToExpected,
      tennis.asInstanceOf[AnySingleConclusionEngine[(Int, Int), String]],
      categoriserEngine.asInstanceOf[AnySingleConclusionEngine[LegacyData[Int, (Int, Int), String], String]],
      reporter, descFn)
    val report = LegacyReport("Tennis", legacy, reporter)
    val html = Report.html(report, LegacyReportRenderer.legacyRenderer)
    println(html)
    Files.printToFile(new File("c:\\users\\phil\\desktop\\test.html"))((p) => p.write(html))
    println("Done")
  }

}