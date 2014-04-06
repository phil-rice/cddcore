package org.corecdd.example

import org.cddcore.engine.tests._
import org.junit.runner.RunWith
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.junit.runner.RunWith
import org.cddcore.engine.Engine
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.example.trader.Trader

/**
 * This class will be swept up by JUnit. It should access all the engines that you want to check
 *  It would be nice if it could be just done by reflection but there are issues with it: objects don't get checked by JUnit, Some engines are created in places with funny constructors...
 */
@RunWith(classOf[CddContinuousIntegrationRunner])
class ExampleContinuousIntegrationTest extends CddContinuousIntegrationTest {
  val engines = List(
    CategorisePerson.categorise,
    CategorisePerson.categorise.cached,
    TennisScorer.scorer,
    org.cddcore.example.processCheque_DM_1.ProcessCheque.processCheque,
    org.cddcore.example.processCheque_DM_2.ProcessCheque.processCheque,
    org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml.processCheque,
    Trader.shouldSell)
}
