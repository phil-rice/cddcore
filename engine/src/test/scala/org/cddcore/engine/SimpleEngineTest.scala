package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SimpleEngineTest extends AbstractTest {
  
  "The simplest engine" should "build" in {
    val b = Engine[Int,Int]().scenario(0).expected(0)
    val e = b.build
    
  }
  
}