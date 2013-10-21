package org.cddcore.engine

import java.io.FileOutputStream

object Files {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  def appendToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(new FileOutputStream(f, true))
	  try { op(p) } finally { p.close() }
  }
}