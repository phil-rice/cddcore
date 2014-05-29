package org.cddcore.engine

import org.junit.runner.RunWith;
import org.scalatest.junit.JUnitRunner

import org.cddcore.engine.builder._

@RunWith(classOf[JUnitRunner])
class DecisionTreeSmokeTest extends AbstractTest {
  import scala.language.implicitConversions
  implicit def toEngineFromTests(e: Engine1[String, String, String]) = e.asInstanceOf[Engine1FromTests[String, String]]
  implicit def todecision[Params, BFn, R, RFn](x: DecisionTreeNode[Params, BFn, R, RFn]) = x.asInstanceOf[Decision[Params, BFn, R, RFn]]
  implicit def toConclusion[Params, BFn, R, RFn](x: DecisionTreeNode[Params, BFn, R, RFn]) = x.asInstanceOf[Conclusion[Params, BFn, R, RFn]]
  val e = Engine[String, String]().
    scenario("A").expected("X").because { _.contains("A") }.
    scenario("AB").expected("Y").because { _.contains("B") }.
    scenario("ABC").expected("Z").because { _.contains("C") }.
    scenario("B").expected("Q").because { _.contains("B") }.
    build
  val evaluator = e.evaluator
  val tree = e.tree
  val d_a = tree.root
  val d_ab = d_a.yes
  val d_abc = d_ab.yes
  val c_y = d_abc.no
  val c_z = d_abc.yes
  val c_x = d_ab.no
  val d_b = d_a.no
  val c_q = d_b.yes
  val c_ex = d_b.no

  val pathToX = c_ex :: d_b :: d_a :: Nil
  val pathToY = c_y :: d_abc :: d_ab :: d_a :: Nil

  "A decision tree findPathToConclusionWithParams method " should "return the conclusion, and it's parent decisions" in {
    assertEquals(pathToX, evaluator.findPathToConclusionWithParams(e.tree, "R"))
    assertEquals(pathToY, evaluator.findPathToConclusionWithParams(e.tree, "AB"))
  }

  "A decision tree findPathToConclusionWithConclusion method passed a conclusion" should "return the conclusion and it's parent decisions" in {
    assertEquals(pathToX, evaluator.findPathToConclusionWithConclusion(tree.root, c_ex, List()))
    assertEquals(pathToY, evaluator.findPathToConclusionWithConclusion(tree.root, c_y, List()))
  }

}
