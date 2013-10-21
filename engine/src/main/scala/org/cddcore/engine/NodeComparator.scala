package org.cddcore.engine

import java.text.MessageFormat

trait NodeComparator[R] extends EngineUniverse[R] {

  def compareNodes(n1: RorN, n2: RorN): List[String] =
    compareNodes("", n1, n2)

  def compareNodes(prefix: String, n1: RorN, n2: RorN): List[String] = {
    n1 match {
      case Left(result1) =>
        n2 match {
          case Left(result2) => compareResults(prefix, result1, result2)
          case Right(node2) => List(prefix + "left is result " + result1.description + " Right is tree " + node2)
        }
      case Right(node1) =>
        n2 match {
          case Left(result2) => List(prefix + "left is tree " + node1 + " Right is result " + result2.description)
          case Right(node2) => compareNodes(prefix, node1, node2)
        }
    }
  }
  def compareResults(prefix: String, c1: CodeAndScenarios, c2: CodeAndScenarios): List[String] = {
    check(prefix + "result {0} {1}", c1.description, c2.description) ++
      compareConstraints(prefix + "scenarios/", c1.scenarios, c2.scenarios)

  }
  def compareNodes(prefix: String, n1: EngineNode, n2: EngineNode): List[String] = {
    check(prefix + "because {0} {1}", n1.becauseString, n2.becauseString) ++
      check(prefix + "inputs {0} {1}", n1.inputs, n2.inputs) ++
      compareNodes(prefix + "yes/", n1.yes, n2.yes) ++
      compareNodes(prefix + "no/", n1.no, n2.no)
  }

  def compareConstraints(prefix: String, c1s: List[Scenario], c2s: List[Scenario]): List[String] = {
    val sizeMismatch = c1s.size != c2s.size match { case true => List(prefix + " sizes " + c1s.size + "," + c2s.size); case _ => List() };
    sizeMismatch ++ (c1s, c2s).zipped.flatMap((c1, c2) => c1 != c2 match { case true => compareConstraint(prefix + "[" + c1.becauseString + "]", c1, c2); case _ => List() });
  }

  def compareConstraint(prefix: String, c1: Scenario, c2: Scenario): List[String] = {
    val b = c1.becauseString != c2.becauseString match { case true => List(prefix + "because " + c1.becauseString + ", " + c2.becauseString); case _ => List() }
    val i = c1.params != c2.params match { case true => List(prefix + "params " + c1.params + ", " + c2.params); case _ => List() }
    val e = c1.expected != c2.expected match { case true => List(prefix + "expected " + c1.expected + ", " + c2.expected); case _ => List() }
    val c = c1.actualCode.description != c2.actualCode.description match { case true => List(prefix + "code " + c1.actualCode.description + ", " + c2.actualCode.description); case _ => List() }
    b ++ i ++ e ++ c
  }
  def compareSize(prefix: String, c1s: List[Scenario], c2s: List[Scenario]): List[String] = {
    if (c1s.size != c2s.size)
      List(prefix + " sizes " + c1s.size + "," + c2s.size);
    else
      List()
  }

  def check[T <: AnyRef](pattern: String, t1: T, t2: T): List[String] = {
    if (t1 == t2)
      List()
    else
      List(MessageFormat.format(pattern, t1, t2))
  }

}
