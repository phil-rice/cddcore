package org.cddcore.eclipse


import scala.language.reflectiveCalls
import java.io.File
import scala.collection.JavaConversions._
import scala.io.Source
import org.autotdd.engine._
import org.autotdd.engine.tests._
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.swt.SWT
import org.eclipse.swt.custom.StyledText
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.part.ViewPart
import org.junit.runner.RunWith
import net.miginfocom.swt.MigLayout
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.graphics.Color
import java.lang.IllegalArgumentException

object AppStateFixture {
  val f1 = new File("1");
  val f2 = new File("2");
  val f3 = new File("3");

  val fct1 = FileContentAndTime(f1, "contentf1", 1)
  val fct1b = FileContentAndTime(f1, "contentf1", 10)
  val fct2 = FileContentAndTime(f2, "contentf2", 1)
  val fct3 = FileContentAndTime(f3, "contentf3", 1)

  val fa1 = new MockFileAccess(fct1, fct2, fct3)
  val fa1b = new MockFileAccess(fct1b, fct2, fct3)

  val as_empty_cache = AppState(fa1, List())
  val as_fct12 = AppState(fa1, List(fct1, fct2))
  val as_fct12_f1_changed_on_file_system = AppState(fa1b, List(fct1, fct2))
}

trait UpdateFiles {
  import AppStateFixture._
  import AppState._
  def makeComposite(color: Int): AutoTddComposite

  type Because = (File, AutoTddComposite, AppState) => Boolean

  val doNothing = (f: File, ac: AutoTddComposite, as: AppState) => as

  val becauseFileIsntInCache: Because = (f, ac, as) => fileCacheL(as, f).isEmpty
  val becauseFileHasChangedOnFileSystem: Because = (f, ac, as) => fileCacheL(as, f).collect { case fct => fct.time != fileAccessL(as)(f).time }.getOrElse(false);
  val becauseFileIsTheSelectedFile: Because = (f, ac, as) => {
    val index = fileCacheL.indexOf(as, f);
    val selection = ac.currentSelection
    index == selection
  }

  def and(b1: Because, b2: Because): Because = (f, ac, as) => b1(f, ac, as) & b2(f, ac, as)

  val testComposite = makeComposite(SWT.COLOR_CYAN)

  val updateGuiAndCacheWhenFileChanges = Engine[File, AutoTddComposite, AppState, AppState]().
    useCase("Default: file is in the cache: do nothing").
    scenario(f1, testComposite, as_fct12).code(doNothing).expected(as_fct12).

    useCase("When file Isn't In cache then add it to list").
    scenario(f1, testComposite, as_empty_cache).
    configuration((f, c, as) => c.reset).
    expected(fileCacheL.set(as_empty_cache, List(fct1))).
    because(becauseFileIsntInCache).
    code((f: File, ac: AutoTddComposite, as: AppState) => {
      val fct = fileAccessL(as)(f)
      val result = fileCacheL.add(as, fct)
      val index = fileCacheL.indexOf(result, f)
      ac.insert(index, fct)
      result
    }).

    useCase("When file has changed, and is not selected item, just update cache").
    scenario(f1, testComposite, as_fct12_f1_changed_on_file_system).
    configuration((f, c, as) => { c.reset; c.insert(0, fct1); c.insert(1, fct2); c.setSelection(1) }).
    expected(fileCacheL.set(as_fct12_f1_changed_on_file_system, List(fct1b, fct2))).
    code((f: File, ac: AutoTddComposite, as: AppState) => {
      val fct = fileAccessL(as)(f)
      val index = fileCacheL.indexOf(as, f)
      fileCacheL.add(as, fct)
    }).because(becauseFileHasChangedOnFileSystem).

    useCase("When file has changed, and is  selected item, update cache and update text area").
    scenario(f1, testComposite, as_fct12_f1_changed_on_file_system).
    configuration((f, c, as) => { c.reset; c.insert(0, fct1); c.insert(1, fct2); c.setSelection(0) }).
    expected(fileCacheL.set(as_fct12_f1_changed_on_file_system, List(fct1b, fct2))).
    code((f: File, ac: AutoTddComposite, as: AppState) => {
      val fct = fileAccessL(as)(f)
      val index = fileCacheL.indexOf(as, f)
      ac.setText(fct.content)
      fileCacheL.add(as, fct)
    }).because(and(becauseFileHasChangedOnFileSystem, becauseFileIsTheSelectedFile)).build;

  //    withLogger(new TestLogger(ClassFunctionList(List(ClassFunction(classOf[File], (f: File) => f.getName())))));

  testComposite.dispose()
}

class AutoTddComposite(parent: Composite, color: Int) extends Composite(parent, SWT.NULL) with LoggerDisplay {
  setLayout(new MigLayout("fill", "[400][grow]", "[grow]"))
  val list = new org.eclipse.swt.widgets.List(this, SWT.WRAP | SWT.READ_ONLY); list.setLayoutData("grow")

  val textArea = new StyledText(this, SWT.WRAP | SWT.READ_ONLY); textArea.setLayoutData("grow")
  def insert(index: Int, fct: FileContentAndTime) =
    try {
      //      println("List: " + list.getItemCount() + " index " + index + ", " + fct.file.getName())
      list.add(fct.file.getName, index)

    } catch {
      case e: Throwable => e.printStackTrace();
    }
  def setSelection(index: Int) = list.setSelection(index)
  def currentSelection: Int = list.getSelectionIndex()
  def reset = list.removeAll()
  def setText(s: String) = textArea.setText(s)
  def loggerDisplay(dp: LoggerDisplayProcessor) = "Comp(sel=" + list.getSelectionIndex() + ", list=" + list.getItems().mkString(",") + ")"
  override def toString =
    {
      val selection = ",selected = " + (if (list.getSelectionIndex() == -1) -1; else list.getSelectionIndex + "/[" + list.getItems()(list.getSelectionIndex()) + "]");
      getClass().getSimpleName() + "(list=" + list.getItems().mkString(",") + selection + ")";
    }
}

object AutoTddComposite {
  def apply(fileAccess: FileAccess, updateFiles: UpdateFiles, parent: Composite) = {
    def directory = AutoTddRunner.directory
    val composite = new AutoTddComposite(parent, SWT.COLOR_BLUE) {
      var appState = new AppState(fileAccess, List())
      list.addSelectionListener(new SelectionListener() {
        override def widgetDefaultSelected(e: SelectionEvent) = {}
        override def widgetSelected(e: SelectionEvent) = {
          val msgs = ""; //updateFiles.updateGuiAndCacheWhenFileChanges.logger.asInstanceOf[TestLogger].messages.mkString("\n")
          list.getSelectionIndex() match {
            case -1 => textArea.setText(msgs);
            case i => textArea.setText(appState.fileCache(i).content + "\n" + msgs)
          }
        }

      })
      val job = new Jobs(getDisplay()).executeRepeatadlyAsJob(2000,
        {
          fileAccess.listFiles.foreach(f =>
            appState = updateFiles.updateGuiAndCacheWhenFileChanges(f, this, appState))
        });
      override def dispose = {
        job.stop = true;
        job.cancel()
        super.dispose
      }
    }
    composite
  }
}
//tests: layout list on left, text area on right. Both filling the space. 
//  when I resize the list and text area should still fill the space
//when the files are updated, then the list model is recreated. Don't care about selection
//when the files are updated the text area receives suitable text. Don't care about selection
//when I click on the list, the text area is populated
//when I start the first item is selected and the text area is populated, if one exists
//If the directory doesn't exist shows <No Engines> in List

@RunWith(classOf[AutoTddRunner])
object ActualUpdateFile {
  lazy val shell = new Shell
//  try {
    val updateFiles = new ActualUpdateFile()
//  } catch {
//    case t: Throwable => {
//      shell.dispose()
//    }
//  }
}

class ActualUpdateFile extends UpdateFiles {
  def makeComposite(color: Int) = new AutoTddComposite(ActualUpdateFile.shell, color)
}

object AutoTddView {
  def main(args: Array[String]) {
    val updateFiles = new ActualUpdateFile()
  }
}

class AutoTddView extends ViewPart {

  def createPartControl(parent: Composite) = {
    try {
      val fileAccess = new FileSystemFileAccess() {
        val userHome = System.getProperty("user.home");
        val directory = new File(userHome, ".autoTdd")
      }

      val updateFiles = new UpdateFiles() {
        def makeComposite(color: Int) = new AutoTddComposite(parent, color)
      }

      val composite = AutoTddComposite(fileAccess, updateFiles, parent);
    } catch {
      case e: Throwable =>
        e.printStackTrace
        throw e;
    }

    println("first")
    //    composite.appState.fileAccess.listFiles.foreach(f => composite.appState = updateFiles.updateGuiAndCacheWhenFileChanges(f, composite, composite.appState))

    //    val job = new Jobs(parent.getDisplay()).
    //      executeRepeatadlyAsJob(2000, fileAccess.listFiles.foreach(f => updateFiles.updateGuiAndCacheWhenFileChanges(f, composite, composittate)));

  }
  def setFocus() = {}
}