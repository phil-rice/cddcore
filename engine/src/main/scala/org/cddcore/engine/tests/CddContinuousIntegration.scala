package org.cddcore.engine.tests

import org.junit.runner.Runner

import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description
import org.junit.runner.RunWith
import org.cddcore.engine._

trait HasEngines {
  def engines: List[Engine]
}

class CddContinuousIntegrationRunner(val clazz: Class[Any]) extends CddRunner {
  def title = "Constraint Driven Development"
  val instance = Engine.test { instantiate(clazz) }.asInstanceOf[HasEngines];
  lazy val enginesToNameMap = Map(instance.engines.map((e) => (e, e.titleOrDescription("<No Name>"))): _*)
}

@RunWith(classOf[CddContinuousIntegrationRunner])
trait CddContinuousIntegrationTest extends HasEngines {
  def engines: List[Engine]
}