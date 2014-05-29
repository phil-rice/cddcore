package org.cddcore.utilities

import java.io.FileOutputStream
import scala.io.Source
import java.io.FileNotFoundException
import java.net.URL
import java.io.OutputStreamWriter

object Files {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    f.delete
    f.getParentFile().mkdirs()
    appendToFile(f)(op)
  } 

  def printToUrl(url: String, text: String, encoding: String = "UTF-8") = {
    try {
      val connection = new URL(url).openConnection();
      connection.setDoOutput(true);
      val out = new OutputStreamWriter(connection.getOutputStream(), encoding);
      try {
        out.write(text);
      } finally {
        out.close();
      }
    } catch { case e: Exception => throw new RuntimeException(s"Url: $url Encoding: $encoding",e) }
  }
  def appendToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new FileOutputStream(f, true))
    try { op(p) } finally { p.close() }
  }

  def getFromClassPath(clazz: Class[_], name: String) = {
    val fullName = clazz.getPackage().getName().replace('.', '/') + "/" + name
    val stream = clazz.getClassLoader().getResourceAsStream(fullName)
    if (stream == null)
      throw new FileNotFoundException(fullName)
    Source.fromInputStream(stream).mkString
  }
  def delete(f: java.io.File) {
    if (f.isDirectory())
      for (c <- f.listFiles())
        delete(c);
    f.delete
    if (f.exists())
      throw new FileNotFoundException("Failed to delete file: " + f);
  }
}