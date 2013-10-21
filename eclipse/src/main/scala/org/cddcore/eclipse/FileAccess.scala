package org.cddcore.eclipse

import java.io.File
import scala.io.Source
import org.autotdd.engine._

trait FileAccess {
  def listFiles: Array[File]
  def apply(f: File): FileContentAndTime
}
trait FileSystemFileAccess extends FileAccess with LoggerDisplay {
  def directory: File
  def apply(f: File) = FileContentAndTime(f, Source.fromFile(f).mkString, f.lastModified())
  def listFiles = directory.listFiles.filter(_.getName.endsWith(".attd"))
  def loggerDisplay(dp: LoggerDisplayProcessor) = "FileAccess(" + dp(directory) + ")"
}

class MockFileAccess(fcts: FileContentAndTime*) extends FileAccess {
  private def addToMap[A, B] = (m: Map[A, B], kv: (A, B)) => m + kv
  val fctMap = fcts.map((fct) => fct.file -> fct).foldLeft(Map[File, FileContentAndTime]())(addToMap)
  def listFiles = fctMap.keys.toArray
  def apply(f: File) = fctMap(f)
}

case class FileContentAndTime(file: File, content: String, time: Long) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = "FCT(" + dp(file) + ")"
}