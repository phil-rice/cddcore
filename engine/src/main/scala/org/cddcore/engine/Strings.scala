package org.cddcore.engine

object Strings {
  val alphas = stringToKvs("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  val digits = stringToKvs("0123456789")
  val brackets = bracketsToKvs("<{(>})", "<<<>>>")
  val misc = stringToKvs("., /=-_")
  val urlMisc = stringToKvs("-_")

  def rawCleaner() = new StringCleaner(Map())
  private val cleaner = new StringCleaner(Map() ++ (alphas ::: digits ::: brackets ::: misc))
  private val urlCleaner = new StringCleaner(Map() ++ (alphas ::: digits ::: urlMisc ::: bracketsToKvs(" ", "_")))

  def clean(s: String): String = cleaner.clean(s)
  def urlClean(s: String): String = urlCleaner.clean(s)

  def stringToKvs(s: String) = s.foldLeft(List[(Char, Char)]()) { (m, c) => (c, c) :: m }
  def bracketsToKvs(left: String, right: String) = left.zip(right).toList

  def oneLine(s: String) = s.replace('\n', ' ')
  def htmlEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("<", "&lt;").replace("&gt;", ">").replace("\n", "<br />")
}

class StringCleaner(map: Map[Char, Char] = Map()) {
  def clean(raw: String): String = {
    val result = raw.flatMap(map.get(_))
    result
  }
}