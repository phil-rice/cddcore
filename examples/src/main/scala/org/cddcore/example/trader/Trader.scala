package org.cddcore.example.trader

import org.cddcore.engine._
import org.cddcore.engine.tests.CddJunitRunner
import org.junit.runner.RunWith

@RunWith(classOf[CddJunitRunner])
object Trader {
  type Stock = String
  type Price = Double
  type Thresholds = Map[Stock, Double]

  val shouldSell = Engine[Stock, Double, Thresholds, Boolean]().
    useCase("Traders want to sell if a stock sells for more than a threshold").
    scenario("Stock", 5, Map("Stock" -> 15), "price is lower than the threshold").expected(false).

    scenario("Stock", 16, Map("Stock" -> 15), "price is higher than the threshold").expected(true).
    because((stock: Stock, price: Price, thresholds: Thresholds) => price >= thresholds(stock)).

    build

}