package org.cddcore.utilities

import scala.collection.immutable.List

object Strings {
  val alphas = stringToKvs("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  val digits = stringToKvs("0123456789")
  val brackets = bracketsToKvs("<{(>})", "<<<>>>")
  val misc = stringToKvs("., /=-_")
  val urlMisc = stringToKvs("-_.")

  def rawCleaner() = new StringCleaner(Map())
  private val cleaner = new StringCleaner(Map() ++ (alphas ::: digits ::: brackets ::: misc))
  private val urlCleaner = new StringCleaner(Map() ++ (alphas ::: digits ::: urlMisc ::: bracketsToKvs(" /", "__")))

  def clean(s: String): String = cleaner.clean(s)
  def urlClean(s: String): String = urlCleaner.clean(s)

  def stringToKvs(s: String) = s.foldLeft(List[(Char, Char)]()) { (m, c) => (c, c) :: m }
  def bracketsToKvs(left: String, right: String) = left.zip(right).toList

  def oneLine(s: String) = s.replace('\n', ' ').replaceAll("\r", " ")
  def htmlEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace("&gt;", ">").replace("\n", "<br />")
  def htmlTooltipEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace("&gt;", ">")
  def url(s: String*) = s.mkString("/")
  def safeToInt(s: String) =
    try {
      Some(s.toInt)
    } catch { case e: NumberFormatException => None }

  def blanks(n: Int) = n match {
    case 0 => ""
    case 1 => " "
    case 2 => "  "
    case 3 => "   "
    case 4 => "    "
    case 5 => "     "
    case _ => List.fill(n)(' ').mkString("")
  }

  def firstCharacters(raw: String) = raw.split("\\s").filter(_.length > 0).map(_(0)).mkString("")
}



class StringCleaner(map: Map[Char, Char] = Map()) {
  def clean(raw: String): String = {
    val result = raw.flatMap(map.get(_))
    result
  }
}