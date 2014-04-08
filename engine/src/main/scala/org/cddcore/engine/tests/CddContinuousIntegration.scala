package org.cddcore.engine.tests

import org.junit.runner.Runner

import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description
import org.junit.runner.RunWith
import org.cddcore.engine._
import org.cddcore.engine.reporting.ReportCreator

trait HasEngines {
  def engines: List[Engine]
}

class CddContinuousIntegrationRunner(val clazz: Class[Any]) extends CddRunner {
  def title = "Constraint Driven Development"
  val instance = Engine.test {
    instantiate(clazz)
  }.asInstanceOf[HasEngines];
  lazy val enginesToNameMap = Map(instance.engines.map((e) => (e, e.titleOrDescription("<No Name>"))): _*)
  val rootEngines = instance.engines
  rootEngines.headOption match {
    case Some(engine) =>
      val packageObject = clazz.getPackage()
      val packageName = if (packageObject == null) "default package" else packageObject.getName()
      val project = Project(packageName + "." + clazz.getSimpleName, rootEngines: _*)
      val report = Report("Junit", project)
      val logger = engine.asInstanceOf[EngineWithLogger].logger
      ReportCreator.fileSystem(logger, report).create
    case _ =>
  }
}

trait CddContinuousIntegrationTest extends HasEngines {
  def engines: List[Engine]
}