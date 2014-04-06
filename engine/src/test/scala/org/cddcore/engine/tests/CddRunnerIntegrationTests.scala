package org.cddcore.engine.tests

import org.cddcore.engine.Engine
import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.runner.notification.RunNotifier
import org.junit.runner.notification.RunListener

object DummyWithEngineAsDef {

  def e = Engine[Int, String]().build

}
@RunWith(classOf[JUnitRunner])
class CddRunnerIntegrationTests extends AbstractTest {

  "An engine defined as a def" should "throw an exception" in {
    val runner = new CddJunitRunner(DummyWithEngineAsDef.getClass())
    val notifier = new RunNotifier
    notifier.addListener(new RunListener() {
      
      
      
    })
    
    
    
    runner.run(notifier)

    
  }
}