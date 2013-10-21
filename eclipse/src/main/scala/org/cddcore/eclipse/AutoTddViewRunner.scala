package org.cddcore.eclipse

import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Display
import java.io.File
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout

object AutoTddViewRunner {

  def main(args: Array[String]) {
    val display = new Display
    val shell = new Shell(display)
    shell.setSize(1300, 500);
    shell.setText("Auto Tdd View Runner");
    shell.setLayout(new FillLayout());
    val updateFiles = new UpdateFiles() {
      def makeComposite(color: Int) = new AutoTddComposite(shell, color)
    }

    val composite = AutoTddComposite(new FileSystemFileAccess() {
      val userHome = System.getProperty("user.home");
      val directory = new File(userHome, ".autoTdd")
    }, updateFiles, shell);

    println("First")
    //    composite.appState.fileAccess.listFiles.foreach(f => composite.appState = updateFiles.updateGuiAndCacheWhenFileChanges(f, composite, composite.appState))
    println("Second")
    //    composite.appState.fileAccess.listFiles.foreach(f => composite.appState = updateFiles.updateGuiAndCacheWhenFileChanges(f, composite, composite.appState))

    //    composite.pack
    shell.pack();
    shell.open();
    while (!shell.isDisposed())
      if (!display.readAndDispatch)
        display.sleep()
  }
}