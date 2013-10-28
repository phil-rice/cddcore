package org.cddcore.engine

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringsTests extends AbstractTest {

  "A StringCleaner" should "only use the characters added to it" in {
    assertEquals("", Strings.rawCleaner.clean("abc123;'#\n~()<>{}\\\\")) //no defined characters
    assertEquals("aQ", new StringCleaner(Map('a'->'a','<'->'Q')).clean("abc123;'#\n~()<>{}\\\\")) //no defined characters

  }
  "The default StringCleaner" should "use alphas digits misc and brackets" in {
    assertEquals("abc123 <><><>.,", Strings.clean("abc123;' #\n~()<>{}.,\\\\")) //no defined characters
  }
  
  "The url cleaner" should "use alphas digits underscores and turn spaces to underscore" in {
	  assertEquals("abc_123_", Strings.urlClean("abc 123;_'#\n~()<>{}\\\\")) //no defined characters
    
  }
}