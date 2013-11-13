import org.cddcore.engine.Engine

object demo {
  //several things to look out for
  //* traces
  //* Folding engines. How to make them easily so other people can too (without modifying core code)
  //* Integration tests: Starting in one engine, calling others in becauses and codes, folding across them ... etc
  //* Multiple true/XML transformation 

  //is a use case basically a reference?
  //if so ... "1.2" and !.2.1 are already handledr
  val useCase: String = "This is a use case object"
  val integrationTest1: String = "This will be an object in a bit"
//  val e1 = Engine[Int, String]().
//    reference("1.2").
//    scenario(0, "x").build
//  val e2 = Engine[Int, String]().build
//  val e3 = Engine[Int, String]().build

  //It may be that assertions on traces work well
  
  
}