package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.htmlRendering.SampleContexts

@RunWith(classOf[JUnitRunner])
class PathUtilsTest extends AbstractTest {
  "The PathUtils findEnginePath" should "return the tail of the path starting with first engine requirement encountered" in {
    import SampleContexts._
    import PathUtils._
    assertEquals(List(eBlankTitleED), PathUtils.findEnginePath(List(eBlankTitleED)))
    assertEquals(List(eBlankTitleED), PathUtils.findEnginePath(List(uc0, eBlankTitleED)))
    assertEquals(List(eBlankTitleED, eBlankReport), PathUtils.findEnginePath(List(uc0, eBlankTitleED, eBlankReport)))
  }
  it should "throw exception if engine requirement  not in path" in {
    import SampleContexts._
    import PathUtils._
    evaluating { PathUtils.findEnginePath(List(uc0)) } should produce[IllegalArgumentException]
  }
  "The PathUtils findEngine" should "return the first engine requirement encountered" in {
    import SampleContexts._
    import PathUtils._
    assertEquals(eBlankTitleED, PathUtils.findEngine(List(eBlankTitleED)))
    assertEquals(eBlankTitleED, PathUtils.findEngine(List(uc0, eBlankTitleED)))
    assertEquals(eBlankTitleED, PathUtils.findEngine(List(uc0, eBlankTitleED, eBlankReport)))
  }
  it should "throw exception if engine requirement  not in path" in {
    import SampleContexts._
    import PathUtils._
    evaluating { PathUtils.findEngine(List(uc0)) } should produce[IllegalArgumentException]
  }

  "The PathUtils findUseCasePath" should "return the tail of the path starting with first usecase encountered" in {
    import SampleContexts._
    import PathUtils._
    assertEquals(List(uc0, eBlankTitleED), PathUtils.findUseCasePath(List(uc0, eBlankTitleED)))
    assertEquals(List(uc0, eBlankTitleED), PathUtils.findUseCasePath(List(uc0s0, uc0, eBlankTitleED)))
    assertEquals(List(uc0, eBlankTitleED, eBlankReport), PathUtils.findUseCasePath(List(uc0s0, uc0, eBlankTitleED, eBlankReport)))
  }
  it should "throw exception if engine requirement  not in path" in {
    import SampleContexts._
    import PathUtils._
    evaluating { PathUtils.findUseCasePath(List(uc0s0)) } should produce[IllegalArgumentException]
  }
  "The PathUtils findUseCase" should "return the first usecase  encountered" in {
    import SampleContexts._
    import PathUtils._
    assertEquals(uc0, PathUtils.findUseCase(List(uc0, eBlankTitleED)))
    assertEquals(uc0, PathUtils.findUseCase(List(uc0s0, uc0, eBlankTitleED, eBlankReport)))
  }
  it should "throw exception if usecase not in path" in {
    import SampleContexts._
    import PathUtils._
    evaluating { PathUtils.findUseCase(List(uc0s0)) } should produce[IllegalArgumentException]
  }
}