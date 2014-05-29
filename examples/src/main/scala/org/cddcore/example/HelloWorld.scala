package org.cddcore.example

import org.cddcore.engine.Engine
import org.junit.runner.RunWith
import org.cddcore.tests.CddJunitRunner

@RunWith(classOf[CddJunitRunner])
object HelloWorld {

  val engine = Engine[Int, String]().
    useCase("Returns hello world the requested number of times").
    scenario(1, "Just once").
    expected("Hello World").
    code((i) => List.fill(i)("Hello World").mkString(", ")).
    scenario(2, "Two times").
    expected("Hello World, Hello World").
    scenario(0).
    matchOn { case x => "asdjsd" + x }.
    expected("").
    scenario(-1).
    expected("").

    build;

  def main(args: Array[String]) {
    println(engine(-1))
    println(engine(0))
    println(engine(1))
    println(engine(2))
    println(engine(3))
  }
}