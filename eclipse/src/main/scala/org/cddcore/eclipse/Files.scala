package org.cddcore.eclipse

object Files {
  import java.io.File

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    if (these == null)
      Array();
    else
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def copy(start: String, to: String) {
    val s = new File(start)
    val t = new File(to)
    for (
      f <- recursiveListFiles(s) //
      if f.getName().endsWith(".jar") //
      if (!f.getPath().contains("org.eclipse")) //
      if (!f.getPath().contains("org.osgi")) //
      if (!f.getPath().contains("servlet-api"))
    ) {
      val d = new File(t, f.getName())
      println("Copying " + f + " to " + d);
      copy(f, d)
    }
  }

  def copy(sourceFile: File, destFile: File) {
    import java.nio.channels.FileChannel
    import java.io.FileInputStream
    import java.io.FileOutputStream

    if (!destFile.exists())
      destFile.createNewFile();

    val source = new FileInputStream(sourceFile).getChannel();
    val destination = new FileOutputStream(destFile).getChannel();
    destination.transferFrom(source, 0, source.size());
  }

  def main(args: Array[String]) {
    copy("../lib_managed", "libFromSbt")
  }
}