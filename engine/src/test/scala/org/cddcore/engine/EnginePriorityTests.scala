package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EnginePriorityTests extends EngineStringStringTests {

  "An Engine" should "allow the priority to set the rules order" in {
    def build(a: Int, b: Int, c: Int) =
      builder.useCase("U").
        withDefaultCode((x: String) => "U").
        scenario("A").because("A").expected("FromA").priority(a).
        scenario("B").because("B").expected("FromB").priority(b).
        scenario("C").because("C").expected("FromC").priority(c).
        build
    assertEquals("if(A)\n FromA:U[0]\nelse\n if(B)\n  FromB:U[1]\n else\n  if(C)\n   FromC:U[2]\n  else\n   \"U\":\n", build(3, 2, 1).toString)
    assertEquals("if(C)\n FromC:U[2]\nelse\n if(B)\n  FromB:U[1]\n else\n  if(A)\n   FromA:U[0]\n  else\n   \"U\":\n", build(1, 2, 3).toString)
  }
}