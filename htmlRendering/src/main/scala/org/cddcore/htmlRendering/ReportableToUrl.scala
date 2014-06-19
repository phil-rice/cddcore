package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine._

trait UrlMap {
  def rootUrl: String
  def rToName: KeyedMap[String]
  def toUrl: KeyedMap[String]
  def fromUrl: Map[String, List[Reportable]]
  def seen: Set[String]

  /** From a reportable to the Url representing it */
  def apply(r: Reportable): String = toUrl(r)
  /** From a reportable to the optional Url representing it */
  def get(r: Reportable): Option[String] = toUrl.get(r)

  /** From a url to the reportable that should be at that url */
  def apply(url: String) = fromUrl(url)
  /** From a url to the optional reportable that should be at that url */
  def get(url: String) = fromUrl.get(url)

  def getName(r: Reportable): String = rToName(r)
  /** Has the reportable got a url? */
  def contains(r: Reportable) = toUrl.contains(r)
  def containsName(s: String) = seen.contains(s)
  def reportableCount = toUrl.size
}

object UrlMap {
  def apply(rootUrl: String = "") = new SimpleReportableToUrl(rootUrl)
  def urlId(r: Reportable)(implicit conv: TemplateLike[Reportable]) = s"${conv(r)}_${r.textOrder}"
}

trait ReportableToUrl[RU <: ReportableToUrl[RU]] extends UrlMap {
  import EngineTools._
  import ReportableHelper._
  def copy(rootUrl: String, rToName: KeyedMap[String], toUrl: KeyedMap[String], fromUrl: Map[String, List[Reportable]], seen: Set[String]): RU
  private def asRu = this.asInstanceOf[RU]
  def addPath(path: List[Reportable])(implicit conv: TemplateLike[Reportable]): (RU, String) = { val result = this + path; (result, result(path.head)) }

  def ++(holder: NestedHolder[Reportable] with Reportable): RU = this ++ holder.pathsIncludingSelf
  def ++(paths: Traversable[List[Reportable]])(implicit conv: TemplateLike[Reportable]): RU = paths.foldLeft(asRu) { _ + _ }
  def ++(engines: List[Engine]): ReportableToUrl[RU] = engines.foldLeft(this)((urlMap, e) => urlMap ++
    e.asRequirement.pathsIncludingTreeAndEngine(List()) ++
    e.asRequirement.documents.map(List(_)))
  def +(path: List[Reportable])(implicit conv: TemplateLike[Reportable]): RU = {
    def url(ru: RU, rToName: KeyedMap[String], path: List[Reportable]) =
      (ru.rootUrl :: path.reverse.map((r) => rToName(r))).mkString("/") + "." + conv(path.head) + ".html"

    def addOnePath(ru: RU, path: List[Reportable]): RU = {
      val head = path.head
      if (head.isInstanceOf[ReportableWithoutUrl])
        return ru

      ru.toUrl.get(head) match {
        case Some(oldUrl) if oldUrl == url(asRu, ru.rToName, path) => ru
        case Some(oldUrl) => 
          throw new IllegalStateException(s"Existing path $oldUrl\nNew path ${url(ru, rToName, path)}\n$path\n")
        case _ =>
          {
            val (newSeen, newRToName) = path.foldLeft((ru.seen, ru.rToName))((acc, r) => acc match {
              case (accSeen, accRToName) =>
                accRToName.contains(r) match {
                  case true => acc
                  case false => {
                    val n = ru.newName(accSeen, r)(conv);
                    (accSeen + n, accRToName + (r -> n))
                  }
                }
            })

            val newUrl = url(ru, newRToName, path);
            if (ru.fromUrl.contains(newUrl))
              throw new IllegalStateException
            val newToUrl = ru.toUrl + (head -> newUrl)
            val newFromUrl = ru.fromUrl + (newUrl -> path)
            ru.copy(ru.rootUrl, newRToName, newToUrl, newFromUrl, newSeen)
          }
      }
    }
    Lists.decreasingList(path).foldLeft(this.asInstanceOf[RU])((acc, p) => addOnePath(acc, p))
  }

  protected def newName(seen: Set[String], r: Reportable)(implicit conv: TemplateLike[Reportable]) = {
    val calculatedName = Strings.urlClean(r match {
      case report: Report => { val result = report.titleOrDescription(""); if (result.length > 120) "" else result }
      //      case project: Project => { val result = project.titleOrDescription(""); if (result.length > 120) "" else result }
      case ed: EngineRequirement[_, _] => { val result = ed.titleOrDescription(""); if (result.length > 120) "" else result }
      case req: Requirement => { val result = req.titleOrDescription(""); if (result.length > 40) "" else result }
      case _ => "";
    }).replace(" ", "_")

    var reqId = r.textOrder
    def makeNewName: String = {
      val templateName = conv(r)
      val name = templateName + reqId;
      reqId += 1
      if (seen.contains(name))
        makeNewName
      else
        name
    }
    if (calculatedName == "" || seen.contains(calculatedName))
      makeNewName
    else
      calculatedName
  }

}

case class SimpleReportableToUrl(rootUrl: String = "", rToName: KeyedMap[String] = new KeyedMap[String](), toUrl: KeyedMap[String] = new KeyedMap[String](), fromUrl: Map[String, List[Reportable]] = Map(), seen: Set[String] = Set()) extends ReportableToUrl[SimpleReportableToUrl] {
  def copy(rootUrl: String, rToName: KeyedMap[String], toUrl: KeyedMap[String], fromUrl: Map[String, List[Reportable]], seen: Set[String]) =
    new SimpleReportableToUrl(rootUrl, rToName, toUrl, fromUrl, seen)
}

