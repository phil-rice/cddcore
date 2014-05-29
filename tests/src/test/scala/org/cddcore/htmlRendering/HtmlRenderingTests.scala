package org.cddcore.htmlRendering

import org.cddcore.tests.CddContinuousIntegrationTest
import org.junit.runner.RunWith
import org.cddcore.tests.CddContinuousIntegrationRunner
import org.cddcore.tests.CddJunitRunner

@RunWith(classOf[CddContinuousIntegrationRunner])
class HtmlRenderingTests extends CddContinuousIntegrationTest {

  //  val engines = List(HtmlRenderer.engineReport)
  val engines = List(
    HtmlRenderer.icon,
    HtmlRenderer.linkAndIcon,
    HtmlRenderer.titleAndIcon,
    HtmlRenderer.engineAndDocumentsSingleItemRenderer,
    HtmlRenderer.engineReportSingleItemRenderer,
    HtmlRenderer.traceReportSingleItemRenderer,
    HtmlRenderer.conclusionPath,
    SampleContexts.folding,
    SampleContexts.eBlank,
    SampleContexts.eBlankTitle,
    SampleContexts.eWithUsecasesAndScenarios)
}

@RunWith(classOf[CddJunitRunner])
class HtmlRenderingUnitTests {
  val e = SampleContexts.eBlank
  val icon = HtmlRenderer.icon
  val linkAndIcon = HtmlRenderer.linkAndIcon
  val titleAndIcon = HtmlRenderer.titleAndIcon
  val engineAndDocumentsSingleItemRenderer = HtmlRenderer.engineAndDocumentsSingleItemRenderer
  val engineReportSingleItemRenderer = HtmlRenderer.engineReportSingleItemRenderer
  val traceReportSingleItemRenderer = HtmlRenderer.traceReportSingleItemRenderer
  val conclusionPath = HtmlRenderer.conclusionPath

}