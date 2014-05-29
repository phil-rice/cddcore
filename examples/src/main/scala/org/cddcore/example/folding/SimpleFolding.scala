package org.cddcore.example.folding

import org.junit.runner.RunWith
import org.cddcore.engine.Engine
import org.cddcore.tests.CddJunitRunner

@RunWith(classOf[CddJunitRunner])
object SimpleFolding {
 val folding = Engine.foldList[Int, Int].title("Folding Engine Title").description("laskdj asdf asd sadf sadf asdf adsf sadf sadf sadf asdf sdfsdfdsfasd sdaf sdf ").
    childEngine("ce0", "Some descr").useCase("uc0").scenario(0).expected(0).
    childEngine("ce1").useCase("uc0").scenario(1).expected(2).code { (x: Int) => x * 2 }.because { (x: Int) => x > 0 }.
    scenario(2).expected(4). 
    build
}