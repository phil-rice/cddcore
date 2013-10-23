package org.cddcore.engine

import java.io.FileOutputStream
import scala.io.Source

object Files {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  def appendToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new FileOutputStream(f, true))
    try { op(p) } finally { p.close() }
  }

  def getFromClassPath(clazz: Class[_], name: String) = {
    val fullName = clazz.getPackage().getName().replace('.', '/') + "/" + name
    val stream = clazz.getClassLoader().getResourceAsStream(fullName)
    Source.fromInputStream(stream).mkString
  }
}