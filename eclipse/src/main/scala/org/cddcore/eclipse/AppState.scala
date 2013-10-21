package org.cddcore.eclipse

import java.io.File
import org.autotdd.engine.LoggerDisplay
import org.autotdd.engine.LoggerDisplayProcessor

case class AppState(fileAccess: FileAccess, fileCache: List[FileContentAndTime])  extends LoggerDisplay {
  def loggerDisplay (dp: LoggerDisplayProcessor)= "App(" + dp(fileAccess) + "," + fileCache.map(dp).mkString(",") + ")";
}

object AppState {
  val fileAccessL = Lens[AppState, FileAccess](_.fileAccess, (as, fa) => as.copy(fileAccess = fa));
  val fileCacheL = new Lens[AppState, List[FileContentAndTime]](_.fileCache, (as, fc) => as.copy(fileCache = fc.sortBy((fct) => fct.file.getName))) {
    def indexOf(as: AppState, f: File): Int = get(as).indexWhere(_.file == f)
    def add(as: AppState, fct: FileContentAndTime) = {
      val index = indexOf(as, fct.file);
      if (index == -1)
        set(as, fct :: get(as))
      else
        set(as, get(as).patch(index, List(fct), 1))
    }
    def apply(as: AppState, f: File) = get(as).find(_.file == f)
  }
}
