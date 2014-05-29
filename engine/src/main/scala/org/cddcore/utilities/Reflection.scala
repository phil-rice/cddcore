package org.cddcore.utilities

object Reflection {
  def instantiate(clazz: Class[_]): Any = {
    import scala.reflect.runtime.{ universe => ru }
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val declaredFields = clazz.getDeclaredFields().toList
    val moduleField = declaredFields.find(field => field.getName() == "MODULE$")
    try {
      val obj = moduleField match {
        case Some(modField) => modField.get(clazz)
        case None => clazz.newInstance()
      }
      obj
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Class: $clazz Field: $moduleField", e);
    }
  }
}