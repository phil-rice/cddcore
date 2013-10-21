package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineLoggerCompileTests extends EngineStringStringTests {
  override val logger = new TestLogger()

  it should "log adding to no clause if because is false for root" in {
    val bldr = builder.useCase("UseCase").
      scenario("W").expected("Z").
      scenario("A").expected("X").because("A").
      scenario("B").expected("Y").because("B")
    logger.reset
    bldr.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Adding A as first if then else",
      "DEBUG Compile() Adding B under no of node A")
  }
  
  it should "use the scenario description if it exists" in { 
    val bldr = builder.useCase("UseCase").
      scenario("W", "w").expected("Z").
      scenario("A", "a").expected("X").because("A").
      scenario("B", "b").expected("Y").because("B")
    logger.reset
    bldr.build
    checkMessages(
      "DEBUG Compile() Adding w as new root",
      "DEBUG Compile() Adding a as first if then else",
      "DEBUG Compile() Adding b under no of node a")
  }
  
  

  "An empty engine" should "Add scenario to root if adding assertion" in {
    val bldr = builder.useCase("UseCase").
      scenario("W").expected("Z").
      scenario("A").expected("Z")
    logger.reset

    bldr.build
    checkMessages("DEBUG Compile() Adding W as new root", "DEBUG Compile() Adding A as extra scenario for Z")
  }

  it should "log the change from null to a root with one scenario" in {
    val b = builder.useCase("UseCase").scenario("W").expected("Z")
    logger.reset
    b.build
    checkMessages("DEBUG Compile() Adding W as new root")
  }

  "An empty engine" should "log the change from null root to if then with two scenarios" in {
    val bldr = builder.useCase("UseCase").
      scenario("W").expected("Z").
      scenario("A").expected("X").because("A")
    logger.reset
    bldr.build
    checkMessages("DEBUG Compile() Adding W as new root", "DEBUG Compile() Adding A as first if then else")
  }

  it should "log adding to yes clause if because is true for root" in {
    val bldr = builder.useCase("UseCase").
      scenario("W").expected("Z").
      scenario("A").expected("X").because("A").
      scenario("AB").expected("Y").because("B")
    logger.reset
    bldr.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Adding A as first if then else",
      "DEBUG Compile() Adding AB under yes of node A")
  }

  it should "log correctly when three scenarios are added" in {
    val bldr = builder.useCase("UseCase").
      scenario("W").expected("Z").
      scenario("A").expected("X").because("A").
      scenario("B").expected("Y").because("B").
      scenario("AB").expected("Z").because("AB")
    logger.reset
    bldr.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Adding A as first if then else",
      "DEBUG Compile() Adding B under no of node A",
      "DEBUG Compile() Adding AB under yes of node A")
  }

  it should "log adding an or rule" in {
    val b = builderWithDefault.
      scenario("A", "a").because("A").expected("W").
      scenario("AB", "ab").because("B").expected("W")
    logger.reset
    val e = b.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Adding a as first if then else",
      "DEBUG Compile() Merging ab under yes of a")
  }

  it should "log merging with root" in {
    val b = builderWithDefault.scenario("B").because("B").expected("Z") //The B is totally redundant in this case. As everything gets to be Z.
    logger.reset
    val e = b.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Merging B into root")
  }

  it should "make an or rule even if several deep" in {
    val b = builderWithDefault.
      scenario("AB", "a").because("A").expected("X").
      scenario("AB", "b").because("B").expected("X");

    logger.reset
    val e = b.build
    checkMessages(
      "DEBUG Compile() Adding W as new root",
      "DEBUG Compile() Adding a as first if then else",
      "DEBUG Compile() Merging b under yes of a")
  }

}