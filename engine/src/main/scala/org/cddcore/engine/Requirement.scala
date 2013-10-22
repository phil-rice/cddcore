package org.cddcore.engine

 case class Reference(ref: String = "", document: String = "")

trait Requirement {
	def title: Option[String]
    def description: Option[String]
    def priority: Int
    def references: List[Reference]
}

trait RequirementHolder extends Requirement {
  def children: List[Requirement]
}