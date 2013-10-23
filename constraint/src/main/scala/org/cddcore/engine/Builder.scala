
import org.cddcore.engine.LensTrait

class LatestLens[T](childrenFn: (T) => Option[List[T]], copyWithChildrenFn: (T, List[T]) => T) extends LensTrait[T, T] {
  def get = (t: T) =>
    childrenFn(t) match {
      case Some(h :: tail) => get(h)
      case _ => t
    }
  def set = (main, newValue) =>
    childrenFn(main) match {
      case Some(h :: tail) => copyWithChildrenFn(main, set(h, newValue) :: tail)
      case _ => newValue
    }
    
}