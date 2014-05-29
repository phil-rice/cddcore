package org.cddcore.utilities

object Exceptions {

  def apply[T](stuff: => T): Either[Exception, T] = try { Right(stuff) } catch { case e: Exception => Left(e) }
  def apply[T, E](stuff: => T, fn: (Exception) => E): Either[E, T] =
    try { Right(stuff) } catch {
      case e: Exception => 
        Left(fn(e));
      case e: ThreadDeath => throw e;
      case e: Throwable => 
        Left(fn(new RuntimeException("Wrapped exception", e)))
    }

}