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
  
  "The blanks method" should "return the correct number of blanks" in {
    assertEquals("", Strings.blanks(0))
    assertEquals(" ", Strings.blanks(1))
    assertEquals("  ", Strings.blanks(2))
    assertEquals("   ", Strings.blanks(3))
    assertEquals("    ", Strings.blanks(4))
    assertEquals("      ", Strings.blanks(6))
  }
  
  "The firstCharacters method" should "return the first characters of each word" in {
    assertEquals("", Strings.firstCharacters(""));
    assertEquals("A", Strings.firstCharacters("A"));
    assertEquals("a", Strings.firstCharacters("a"));
    assertEquals("apoc", Strings.firstCharacters("a piece of cake"));
  } 
}