package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EnginePriorityTests extends EngineStringStringTests {

  "An Engine" should "allow the priority to set the rules order" in {
    def build(a: Int, b: Int, c: Int) =
      builder.code((x: String) => "U").
        useCase("U").
        scenario("A").because("A").expected("FromA").priority(a).
        scenario("B").because("B").expected("FromB").priority(b).
        scenario("C").because("C").expected("FromC").priority(c).
        build
    assertEquals("if(A)\n FromA:A\nelse\n if(B)\n  FromB:B\n else\n  if(C)\n   FromC:C\n  else\n   \"U\":\n", build(3, 2, 1).toString)
    assertEquals("if(C)\n FromC:C\nelse\n if(B)\n  FromB:B\n else\n  if(A)\n   FromA:A\n  else\n   \"U\":\n", build(1, 2, 3).toString)
  }
}