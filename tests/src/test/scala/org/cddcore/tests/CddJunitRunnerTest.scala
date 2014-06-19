package org.cddcore.tests

import org.cddcore.engine.AbstractTest
import org.cddcore.engine.Engine
import org.junit.runner.JUnitCore


class CddJunitRunnerTest extends AbstractTest {

  "A test run" should "terminate when one of the tests fails" in {
    JUnitCore.runClasses(SampleBrokenEngine.getClass())
  }
}