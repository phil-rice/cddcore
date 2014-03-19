package org.corecdd.example

import org.cddcore.engine.tests._
import org.junit.runner.RunWith
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.junit.runner.RunWith
import org.cddcore.engine.Engine

/**
 * This class will be swept up by JUnit. It should access all the engines that you want to check
 *  It would be nice if it could be just done by reflection but there are issues with it: objects don't get checked by JUnit, Some engines are created in places with funny constructors...
 */
@RunWith(classOf[CddContinuousIntegrationRunner])
class ExampleContinuousIntegrationTest extends CddContinuousIntegrationTest {
  val engines = List(
    CategorisePerson.categorise)
}

object check {
  def main(args: Array[String]) {

  } 
}