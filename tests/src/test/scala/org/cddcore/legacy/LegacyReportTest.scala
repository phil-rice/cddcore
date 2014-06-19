package org.cddcore.legacy

import org.cddcore.tests.CddContinuousIntegrationTest
import org.junit.runner.RunWith
import org.cddcore.tests.CddContinuousIntegrationRunner
import org.cddcore.tests.CddJunitRunner

@RunWith(classOf[CddContinuousIntegrationRunner])
class LegacyReportTest extends CddContinuousIntegrationTest {

  val engines = List(LegacyReportRenderer.legacyRenderer)
}

 