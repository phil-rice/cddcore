package org.cddcore.example

import org.junit.runner.RunWith
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.tests.CddContinuousIntegrationRunner
import org.cddcore.tests.CddContinuousIntegrationTest

/**
 * This class will be swept up by JUnit. It should access all the engines that you want to check
 *  It would be nice if it could be just done by reflection but there are issues with it: objects don't get checked by JUnit, Some engines are created in places with funny constructors...
 */
@RunWith(classOf[CddContinuousIntegrationRunner])
class ExampleJunitSuite extends CddContinuousIntegrationTest {

  def engines = List(TennisScorer.scorer)

}