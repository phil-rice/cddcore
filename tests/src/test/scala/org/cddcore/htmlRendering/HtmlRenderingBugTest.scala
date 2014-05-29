package org.cddcore.htmlRendering

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._
import org.cddcore.utilities.Strings
import org.cddcore.website.ExampleWebSiteEngines
import java.util.Date

@RunWith(classOf[JUnitRunner])
class HtmlRenderingBugTest extends AbstractTest with ExampleWebSiteEngines {

  "An engine " should "deal with the bug that isn't categorized yet" in {
    import SampleContexts._
    import HtmlRenderer._
    val titleAndIcon = Engine[RenderContext, Reportable, String]().title("titleAndIcon").description("Finds a suitable titleAndIcon for a reportable. Includes links to go to item, and the id from the urlmap").
      useCase("Items that are requirements with titles use their titles").
      scenario(context(reqWithTitleReport), reqWithTitle).
      expected(s"<a id='RequirementForTest_${reqWithTitle.textOrder}' href='RootUrl/ReportTitle/ReqTitle.RequirementForTest.html'>ReqTitle<!-- no icon --></a>").
      matchOn { case (rc, r: Requirement) if r.title.isDefined => s"<a id='${UrlMap.urlId(r)}' href='${rc.urlMap(r)}'>${Strings.htmlEscape(r.titleString)}${icon(rc, r)}</a>" }.

      useCase("Items that are requirements without titles are given template name and text order").
      scenario(doc1NoTitlereport, docNoTitle).
      expected { val d = s"Document_${docNoTitle.textOrder}"; s"<a id='$d' href='RootUrl/doc1Report/Document${docNoTitle.textOrder}.Document.html'>$d${icon(doc1NoTitlereport, docNoTitle)}</a>" }.
      matchOn { case (rc, r: Requirement) => s"<a id='${UrlMap.urlId(r)}' href='${rc.urlMap(r)}'>${UrlMap.urlId(r)}${icon(rc, r)}</a>" }.

      useCase("Engines are displayed based on their requirements. Without a name uses template name and text order").
      scenario(eBlankTitleReport, eBlankTitleED).
      expected { s"<a id='EngineDescription_${eBlankTitleED.textOrder}' href='RootUrl/engineReportTitle/EBlankTitle.EngineDescription.html'>EBlankTitle${icon(eBlankTitleReport, eBlankTitleED)}</a>" }.
      matchOn { case (rc, ed: EngineDescription[_, _, _, _]) => s"<a id='${UrlMap.urlId(ed)}' href='${rc.urlMap(ed)}'>${Strings.htmlEscape(ed.titleString)}${icon(rc, ed)}</a>" }.

      build
    val rc = RenderContext(UrlMap() ++ engines, new Date, "")
    for (e <- engines)
      titleAndIcon(rc, e.asRequirement)
  }
}