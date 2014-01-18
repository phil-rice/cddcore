package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class SimpleRequirementAndHolderTests extends AbstractTest {

  "A SimpleRequirementAndHolder" should "wrap Reports, Engines, Usecases but not Scenarios" in {
    val eMain = Engine[Int, String]().title("Main Engine").
      useCase("UC 1", "The body of the first use case").priority(3).
      scenario(11).expected("one-one").because((x: Int) => x == 11).
      build
    val useCase = eMain.all(classOf[UseCase])(0)
    val scenario = eMain.all(classOf[Test])(0)
    val report = Report("some title", eMain)
    assertEquals(report, SimpleRequirementAndHolder(report).asInstanceOf[SimpleRequirementAndHolder].delegate.get)
    assertEquals(eMain, SimpleRequirementAndHolder(eMain).asInstanceOf[SimpleRequirementAndHolder].delegate.get)
    assertEquals(useCase, SimpleRequirementAndHolder(useCase).asInstanceOf[SimpleRequirementAndHolder].delegate.get)
    assertEquals(scenario, SimpleRequirementAndHolder(scenario))
  }

}