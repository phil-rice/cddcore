package org.cddcore.website

import org.cddcore.engine._
import EngineTools._

import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer
import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import junit.textui.TestRunner

trait ExampleWebSiteEngines {
  lazy val blankEngine = Engine[Int, String]().build
  lazy val engines = List(
    CategorisePerson.categorise,
    ProcessChequeXml.processCheque,
    blankEngine,
    TennisScorer.scorer)

}

@RunWith(classOf[JUnitRunner])
class ExampleWebsiteTest extends AbstractWebsiteTest with ExampleWebSiteEngines {
  import Reportable._

  "The example website" should "display the project page if no page specified" in {
    projectPageAtIndex
  }

  it should "go to the project page if cdd logo clicked" in {
    projectPageAtIndex.clickLogo
  }

  def clickAllLinks[Main <: AbstractPage, Checked <: AbstractPage, R <: Reportable](
    startPage: Main,
    reportableListGetter: (Main) => List[List[Reportable]],
    click: (Main, List[Reportable]) => Checked,
    recover: (Checked) => Main) = {

    var here = startPage
    for (r <- reportableListGetter(startPage)) {
      val newPage = click(here, r)
      here = recover(newPage)
    }
  }

  def projectPageToEnginePath: (IndexPage) => List[List[Reportable]] = (pp) => engines.map((e)=>List(e.asRequirement))

  it should "allow each engine to be display" in {
    clickAllLinks[IndexPage, EngineFromTestsPage, EngineRequirement[_, _]](projectPageAtIndex,
      projectPageToEnginePath,
      click = _.clickEngineFromTests(_),
      recover = _.clickLogo)
  }

}

 
//  it should "allow the live button to be displayed" in {
//    for (e: Engine <- project.collect { case e: Engine => e }) {
//      projectPageAtIndex
//      val idForE = reportableToUrl.urlId(e, None);
//      click on id(idForE)
//      val enginePage = new EnginePage(List(e, project, report))
//      click on id(urlMap(e) + "/live")
//      val livePage = new LivePage(e)
//      if (livePage.hasForm) {
//        livePage.submit match {
//          case Some(newLivePage) =>
//            import ParamDetails._
//            val params = e.paramDetails.foldLeft(List[Any]())((acc, pd) => pd match { case ParamDetail(_, parser, Some(tv)) => acc :+ parser(tv); case _ => Nil })
//            if (params != Nil)
//              assertTextEquals(e.asInstanceOf[EngineWithResult[_]].applyParams(params).toString, newLivePage.resultTd)
//          case _ => ;
//        }
//
//      }
//    }
//  }

