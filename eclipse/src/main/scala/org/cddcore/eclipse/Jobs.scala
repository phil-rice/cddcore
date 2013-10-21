package org.cddcore.eclipse

import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.swt.widgets.Display

class Jobs(val display: Display) extends NeedsSwtThread {

  def executeAsJob(calculate: => Unit) = {
    val job = new Job("My Job") {
      def run(monitor: IProgressMonitor): IStatus = {
        execute(new Runnable() {
          def run() = {
            println("Should calculate")
            calculate
          }
        })
        Status.OK_STATUS;
      }
    };
    job.schedule()
    job
  }

  def executeRepeatadlyAsJob(period: Int, calculate: => Unit) = {
    println("Setting up job (" + period + ")")
    val job = new Job("My Job") {
      var stop = false
      def run(monitor: IProgressMonitor): IStatus = {
        println("in job run")
        try {
          execute(new Runnable() {
            def run {
              calculate
            }
            override def toString = "Running calculate "
          })
        } finally {
          if (!stop)
            schedule(period)
        }
        Status.OK_STATUS;
      }
    }
    job.schedule()
    job
  }
}