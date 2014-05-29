package org.cddcore.htmlRendering

import org.junit.runner.RunWith
import org.cddcore.engine.AbstractTest
import org.scalatest.junit.JUnitRunner
import org.cddcore.website.ExampleWebSiteEngines
import java.util.Date
import org.cddcore.engine.EngineTools

@RunWith(classOf[JUnitRunner])
class HtmlRenderingIntegrationTest extends AbstractTest with ExampleWebSiteEngines {
  "The titleAndIcon renderer" should "deal with the engines in the example web test" in { //original it didn't..
    import EngineTools._
    val rc = RenderContext(UrlMap() ++ engines, new Date, "")
    val engine = HtmlRenderer.titleAndIcon
    import SampleContexts._
    val ed = eBlankED
    val ted = eBlankTitleED
    engine(context(eBlankTitleReport), ted)
    engine(context(eBlankReport), ed)
    val req = engines(1).asRequirement
    engine(rc, req)

    for (e <- engines)
      engine(rc, e.asRequirement)
  }

  "The html rendered" should "load" in {
    import HtmlRenderer._
    import SampleContexts._
    val report = foldingTraceReport
    val rc: RenderContext = report
    val urlMap = rc.urlMap
    val item = actualFoldingTI
    val x = urlMap.get(item)

        HtmlRenderer.titleAndIcon(rc, actualFoldingTI)
    val html = Report.htmlFromTrace("", trace)
//    println(html)
  }
}