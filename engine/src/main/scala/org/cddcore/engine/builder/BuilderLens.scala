package org.cddcore.engine.builder

import org.cddcore.utilities.Lens
import org.cddcore.engine._
import org.cddcore.utilities.CodeHolder
import org.cddcore.utilities.ExceptionMap

class BuilderLens[Params, R, FullR, B <: BuilderNodeHolder[Params, R]] {

  def builderToCanCopyWithNewExceptionMapL = Lens[B, CanCopyWithNewExceptionMap[R]](
    (b) => b.asInstanceOf[CanCopyWithNewExceptionMap[R]],
    (b, n) => n.asInstanceOf[B],
    Some("toCanCopyExMapL"))

  val currentNodeL: Lens[B, BuilderNode[Params, R]] = Lens[B, BuilderNode[Params, R]](
    (b: B) => currentNodeForHoldersL.get(b),
    (b: B, n: BuilderNode[Params, R]) => { val result = currentNodeForHoldersL.set(b, n).asInstanceOf[B]; result })

  val exceptionMap = Lens[CanCopyWithNewExceptionMap[R], ExceptionMap](
    (c) => c.buildExceptions,
    (c, e) =>
      c.copyWithNewExceptions(e),
    Some("exceptionMap"))

  protected val currentNodeForHoldersL: Lens[BuilderNodeHolder[Params, R], BuilderNode[Params, R]] = Lens[BuilderNodeHolder[Params, R], BuilderNode[Params, R]](
    (b: BuilderNodeHolder[Params, R]) =>
      b.nodes match {
        case (eh: BuilderNodeHolder[Params, R]) :: tail if (!eh.nodes.isEmpty) => currentNodeForHoldersL.get(eh);
        case (n: BuilderNode[Params, R]) :: tail => n
      },
    (enh: BuilderNodeHolder[Params, R], n: BuilderNode[Params, R]) => {
      val result = enh.nodes match {
        case (eh: BuilderNodeAndHolder[Params, R]) :: tail if (!eh.nodes.isEmpty) =>
          enh.copyNodes(nodes = (currentNodeForHoldersL.set(eh, n).asInstanceOf[BuilderNode[Params, R]] :: tail))
        case (_: BuilderNode[Params, R]) :: tail =>
          enh.copyNodes(n :: enh.nodes.tail)
      }
      result
    })

  val nextUseCaseHolderL = Lens[B, BuilderNodeHolder[Params, R]](
    (b) => nextUseCaseHolderForHoldersL.get(b),
    (b, n) => nextUseCaseHolderForHoldersL.set(b, n).asInstanceOf[B])

  val engineDescriptionL: Lens[BuilderNodeHolder[Params, R], EngineDescription[Params, R]] = Lens[BuilderNodeHolder[Params, R], EngineDescription[Params, R]](
    (b: BuilderNodeHolder[Params, R]) => {
      (b, b.nodes) match {
        case (ed: EngineDescription[Params, R], _) => ed
        case (_, (h: BuilderNodeHolder[Params, R]) :: tail) => engineDescriptionL.get(h)
        case x => throw new IllegalArgumentException(x.toString)
      }
    },
    (enh: BuilderNodeHolder[Params, R], n: EngineDescription[Params, R]) => {
      (enh, enh.nodes) match {
        case (ed: EngineDescription[Params, R], _) => n
        case (_, (h: BuilderNodeHolder[Params, R]) :: tail) => enh.copyNodes(engineDescriptionL.set(h, n).asInstanceOf[BuilderNode[Params, R]] :: tail)
        case x => throw new IllegalArgumentException(x.toString)

      }
    })

  protected val nextUseCaseHolderForHoldersL: Lens[BuilderNodeHolder[Params, R], BuilderNodeHolder[Params, R]] = Lens[BuilderNodeHolder[Params, R], BuilderNodeHolder[Params, R]](
    (b: BuilderNodeHolder[Params, R]) => {
      (b, b.nodes) match {
        case (ed: EngineDescription[Params, R], _) => ed
        case (_, (h: BuilderNodeHolder[Params, R]) :: tail) => nextUseCaseHolderForHoldersL.get(h)
        case x => throw new IllegalArgumentException(x.toString)
      }
    },
    (enh: BuilderNodeHolder[Params, R], n: BuilderNodeHolder[Params, R]) => {
      (enh, enh.nodes) match {
        case (ed: EngineDescription[Params, R], _) => n
        case (_, (h: BuilderNodeHolder[Params, R]) :: tail) => enh.copyNodes(nextUseCaseHolderForHoldersL.set(h, n).asInstanceOf[BuilderNode[Params, R]] :: tail)
        case x => throw new IllegalArgumentException(x.toString)
      }
    })

  val nodesL = Lens[BuilderNodeHolder[Params, R], List[BuilderNode[Params, R]]](
    (b) => b.nodes,
    (b, n) => b.copyNodes(nodes = n),
    Some("nodesL"))

  val nextScenarioHolderL = Lens[B, BuilderNodeHolder[Params, R]](
    (b) => nextScenarioHolderForHolderL.get(b),
    (b, n) => nextScenarioHolderForHolderL.set(b, n).asInstanceOf[B])

  protected val nextScenarioHolderForHolderL: Lens[BuilderNodeHolder[Params, R], BuilderNodeHolder[Params, R]] = Lens[BuilderNodeHolder[Params, R], BuilderNodeHolder[Params, R]](
    (b) => b.nodes match {
      case (eh: BuilderNodeHolder[Params, R]) :: tail => nextScenarioHolderForHolderL.get(eh);
      //      case (n: BuilderNodeHolder[Params,R]) :: tail => n
      case _ => b
    },
    (b, n) => {
      val result = b.nodes match {
        case (eh: BuilderNodeHolder[Params, R]) :: tail => b.copyNodes(nodes = nextScenarioHolderForHolderL.set(eh, n).asInstanceOf[BuilderNode[Params, R]] :: tail);
        case _ :: tail => n
        case Nil => n
      }
      result
    })

  val asRequirementL = Lens[BuilderNode[Params, R], Requirement](
    (en) => en,
    (en, t) => t.asInstanceOf[BuilderNode[Params, R]])
  val titleL = Lens.option[Requirement, String](
    (en) => en.title,
    (en, t) => en.copyRequirement(title = t),
    (old, v) => CannotDefineTitleTwiceException(old, v),
    Some("titleL"))
  val descriptionL = Lens.option[Requirement, String](
    (en) => en.description,
    (en, d) => en.copyRequirement(description = d),
    (old, v) => CannotDefineDescriptionTwiceException(old, v),
    Some("descriptionL"))
  val priorityL = Lens.option[Requirement, Int](
    (en) => en.priority,
    (en, p) => en.copyRequirement(priority = p),
    (old, v) => CannotDefinePriorityTwiceException(old, v),
    Some("priorityL"))
  val referencesL = Lens[Requirement, Set[Reference]](
    (en) => en.references,
    (en, references) => en.copyRequirement(references = references))
  val expectedL = Lens.option[BuilderNode[Params, R], Either[Exception, R]](
    (en) => en.expected,
    (en, ex) => en.copyBuilderNode(expected = ex),
    (old, v) => CannotDefineExpectedTwiceException(old, v),
    Some("expectedL"))
  def codeL(validate: (BuilderNode[Params, R], BuilderNode[Params, R], CodeHolder[(Params)=>R]) => Unit) = Lens.option[BuilderNode[Params, R], CodeHolder[(Params)=>R]](
    (b) => b.code,
    (b, cCodeHolder) => b.copyBuilderNode(code = cCodeHolder),
    (old, v) => CannotDefineCodeTwiceException(old, v),
    Some("codeL"),
    Some(validate))
  val toFoldingEngineDescription = Lens[B, FoldingEngineDescription[Params, R, FullR]](
    (b) => b.nodes.head match {
      case f: FoldingEngineDescription[Params, R, FullR] => f
      case _ => throw new CannotHaveChildEnginesWithoutFolderException
    },
    (b, n) => b.copyNodes(nodes = List(n)).asInstanceOf[B],
    Some("toFoldEngine"))
  val foldEngineNodesL = Lens[FoldingEngineDescription[Params, R, FullR], List[BuilderNode[Params, R]]](
    (b) => b.nodes,
    (b, n) => b.copyNodes(nodes = n),
    Some("nodesL"))
}

class FullBuilderLens[Params, R, FullR, B <: BuilderNodeHolder[Params, R]] extends BuilderLens[Params, R, FullR, B] {
  type S = Scenario[Params, R]
  val toScenarioL = Lens[BuilderNode[Params, R], S](
    (b) => b match { case s: S => s; case _ => throw NeedScenarioException() },
    (b, s) => b match { case _: S => s; case _ => throw NeedScenarioException() })
  val assertionL = Lens[S,  List[CodeHolder[(Params, Either[Exception, R]) => Boolean]]](
    (s) => s.assertions,
    (s, a) => s.copyScenario(assertions = a))
  def becauseL(validate: (S, S, CodeHolder[(Params)=>Boolean]) => Unit) = Lens.option[S, CodeHolder[(Params)=>Boolean]](
    (s) => s.because,
    (s, bCodeHolder) => s.copyScenario(because = bCodeHolder),
    (old, v) => CannotDefineBecauseTwiceException(old, v),
    Some("becauseL"),
    Some(validate))
  def configuratorL = Lens[S, List[Params => Unit]](
    (s) => s.configurators,
    (s, c) => s.copyScenario(configurators = c),
    Some("configuratorL"))
}

