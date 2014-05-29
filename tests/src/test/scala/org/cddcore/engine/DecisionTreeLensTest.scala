package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

abstract class DecisionTreeLensTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  val evaluateTree = buildEngine.evaluateTree
  val makeClosures = buildEngine.mc
  val sa = s("A", because = "A", expected = "X")
  val sb = s("B", because = "B", expected = "Y")
  val sc = s("X", because = "X", expected = "Z")
  val sab = s("AB", because = "AB", expected = "XY")

  val bc_a = makeClosures.makeBecauseClosure(sa)
  val bc_b = makeClosures.makeBecauseClosure(sb)
  val bc_ab = makeClosures.makeBecauseClosure(sab)

  val rootA_AB_B = dec(sa, yes = dec(sab, conc(sab), conc(sa)), no = conc(sb))
  val treeA_AB_B = decisionTreeLens.creator(rootA_AB_B)

  val dtLens = decisionTreeLens
  import dtLens._

  "RootL" should "return the root" in {
    val tree = decisionTreeLens.creator(conc(sa))
    assertEquals(conc(sa), rootL.get(tree))

    val newTree = rootL.set(tree, conc(sb))
    assertEquals(decisionTreeLens.creator(conc(sb)), newTree)

  }

  "Find conclusion lens " should "focus on  the root, if the root is the only node in the tree" in {
    val root = conc(sa)
    val tree = decisionTreeLens.creator(conc(sa))
    val lens = evaluateTree.findLensToConclusion(root, bc_a)
    assertEquals(conc(sa), lens.get(tree))

    val newTree = lens.set(tree, conc(sb))
    assertEquals(decisionTreeLens.creator(conc(sb)), newTree)
  }

  it should "focus on the conclusion A" in {
    val lens = evaluateTree.findLensToConclusion(rootA_AB_B, bc_a)
    assertEquals(conc(sa), lens.get(treeA_AB_B))

    val newTree = lens.set(treeA_AB_B, conc(sb))
    val expected = dec(sa, yes = dec(sab, conc(sab), conc(sb)), no = conc(sb))
    assertEquals(expected, newTree.root)
  }

  it should "focus on the conclusion AB" in {
    val lens = evaluateTree.findLensToConclusion(rootA_AB_B, bc_ab)
    val actual = lens.get(treeA_AB_B)
    assertEquals(conc(sab), actual)

    val newTree = lens.set(treeA_AB_B, conc(sc))
    val expected = dec(sa, yes = dec(sab, conc(sc), conc(sa)), no = conc(sb))
    assertEquals(expected, newTree.root)

  }

  it should "focus on the conclusion B" in {
    val lens = evaluateTree.findLensToConclusion(rootA_AB_B, bc_b)
    val actual = lens.get(treeA_AB_B)
    assertEquals(conc(sb), actual)

    val newTree = lens.set(treeA_AB_B, conc(sc))
    val expected = dec(sa, yes = dec(sab, conc(sab), conc(sa)), no = conc(sc))
    assertEquals(expected, newTree.root)

  }

}

abstract class DecisionTreeLens1Test[P, R] extends DecisionTreeLensTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class DecisionTreeLens2Test[P1, P2, R] extends DecisionTreeLensTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class DecisionTreeLens3Test[P1, P2, P3, R] extends DecisionTreeLensTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringTest extends DecisionTreeLens1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringStringTest extends DecisionTreeLens2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringStringStringTest extends DecisionTreeLens3Test[String, String, String, String] with StringStringStringStringTest
