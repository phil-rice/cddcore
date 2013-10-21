package org.cddcore.example

import org.junit.runner.RunWith

import org.cddcore.engine._
import org.cddcore.engine.tests._

@RunWith(classOf[CddJunitRunner])
object Bowling {

  //get returns the ith ball or zero   
  val get = Engine[List[Int], Int, Int]().
    useCase("If index in range of list (i.e. the ball has been rolled, return ith item").
    scenario(List(7, 10, 4, 3), 0, "Start of legal range").expected(7).code((rolls: List[Int], i: Int) => rolls(i), "Returns the ith item from the list").
    scenario(List(7, 10, 4, 3), 3, "End of legal range").expected(3).

    useCase("If index is negative return zero").
    scenario(List(7, 10, 4, 3), -1, "First value below legal range").expected(0).code((rolls: List[Any], i: Int) => 0).because((rolls: List[Any], i: Int) => i < 0, "Anything below zero").
    scenario(List(7, 10, 4, 3), -100, "Quite a lot below legal range").expected(0).

    useCase("If index is too high return zero").
    scenario(List(7, 10, 4, 3), 4, "First value above legal range").expected(0).because((rolls: List[Any], i: Int) => i >= rolls.size).
    scenario(List(7, 10, 4, 3), 100, "Quite a lot above legal range").expected(0).build;

  //    get.validateScenarios

  val makeFrame = Engine[List[Int], Int, Frame]().
    useCase("NormalFrames are produced when the two balls at and after the ith ball don't add up to 10").
      scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0).expected(NormalFrame(7, 2)).code((rolls: List[Int], i: Int) => NormalFrame(get(rolls, i), get(rolls, i + 1))).
    scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 4).expected(NormalFrame(3, 0)).

    useCase("Strike Frames are produced when the ith ball equals 10. They include the ith ball, and the next two balls").
    scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6).expected(StrikeFrame(10, 2, 4): Frame).
    because((rolls: List[Int], i: Int) => get(rolls, i) == 10).
    code(((rolls: List[Int], i: Int) => StrikeFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)))).
    scenario(List(10), 0).expected(StrikeFrame(10, 0, 0)).
    scenario(List(10, 10), 0).expected(StrikeFrame(10, 10, 0)).
    scenario(List(10, 10, 10), 0).expected(StrikeFrame(10, 10, 10)).

    useCase("Spare Frames are produced when the two balls at and after the ith ball add up to 10. They include the two balls, and the next ball").
    scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2).expected(SpareFrame(5, 5, 3): Frame).
    because((rolls: List[Int], i: Int) => get(rolls, i) + get(rolls, i + 1) == 10).
    code((rolls: List[Int], i: Int) => SpareFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)): Frame).
    scenario(List(5, 5), 0).expected(SpareFrame(5, 5, 0)).build;
  
  //  (rolls: List[Int], i: Int) => NormalFrame(get(rolls, i), get(rolls, i + 1)),
  //    List(
  //      UseCase("NormalFrames are produced when the two balls at and after the ith ball don't add up to 10",
  //        Scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0).produces(NormalFrame(7, 2)),
  //        Scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 4).produces(NormalFrame(3, 0))),
  //      UseCase("Strike Frames are produced when the ith ball equals 10. They include the ith ball, and the next two balls",
  //        Scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6).produces(StrikeFrame(10, 2, 4): Frame).
  //          because((rolls: List[Int], i: Int) => get(rolls, i) == 10).
  //          byCalling((rolls: List[Int], i: Int) => StrikeFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2))),
  //        Scenario(List(10), 0).produces(StrikeFrame(10, 0, 0)),
  //        Scenario(List(10, 10), 0).produces(StrikeFrame(10, 10, 0)),
  //        Scenario(List(10, 10, 10), 0).produces(StrikeFrame(10, 10, 10))),
  //      UseCase("Spare Frames are produced when the two balls at and after the ith ball add up to 10. They include the two balls, and the next ball",
  //        Scenario(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2).produces(SpareFrame(5, 5, 3): Frame).
  //          because((rolls: List[Int], i: Int) => get(rolls, i) + get(rolls, i + 1) == 10).
  //          byCalling((rolls: List[Int], i: Int) => SpareFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)): Frame),
  //        Scenario(List(5, 5), 0).produces(SpareFrame(5, 5, 0)))));

  def main(args: Array[String]) {
    println(get.toString)
    println(makeFrame.toString)
  }
}