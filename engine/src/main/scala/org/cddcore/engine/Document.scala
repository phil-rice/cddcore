package org.cddcore.engine

case class Document(name: Option[String] = None, title: Option[String] = None, description: Option[String] = None, url: Option[String] = None){
  def titleString = name.getOrElse(title.getOrElse(url.getOrElse(description.getOrElse(""))))
}

case class Reference(ref: String = "", document: Option[Document] = None) extends Comparable[Reference] {

  def titleString = document.collect { case d => d.titleString + " " }.getOrElse("") + ref
  def compareTo(other: Reference): Int = {
    val left = ref.split("\\.")
    val right = other.ref.split("\\.")
    val zipped = left.zipAll(right, "0", "0")
    zipped.map((f) => {
      val (l, r) = f
      try {
        val lInt = l.toInt
        val rInt = r.toInt
        lInt - rInt
      } catch {
        case e: Throwable => {
          l.compareTo(r)
        }
      }
    }).find((f) => f != 0).getOrElse(0)
  }
}

case class RefTree(ref: Reference, children: List[Reference])

case class Project(projectTitle: String, engines: Engine*) extends RequirementHolder {
  lazy val documents = engines.flatMap(_.documents).distinct
  lazy val refToRequirement: Map[Reference, Requirement] =
    engines.foldLeft(List[(Reference, Requirement)]())((acc, e) => acc ++
      e.flatMap(_.references.map((_, e))) ++
      e.references.map((_, e))).toMap

  val title = Some(projectTitle)
  val children = engines.toList
  def description = None
  def priority = 0
  def references = List()
}

