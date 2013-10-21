package org.cddcore.engine
import scala.language.experimental.macros
import scala.reflect.macros.Context
//object Demo {
//
//  def applyUsingVarArgs(y: Any, args: Any*) = new Demo(y, args.toList)
//  def applyUsingListArgs(y: Any,args: List[Any]) = new Demo(y, args)
//
//  def apply_impl(c: Context)(x: c.Expr[Any], args: c.Expr[Any]*): c.Expr[Unit] = {
//    import c.universe._
//    reify {
//      val y = x;//would actually be some complicated processing here
//      applyUsingVarArgs(y, args.splice.toList) //only need this OR the next line
//      applyUsingListArgs(y, args.splice)
//    }
//  }
//  def apply(x: Any, args: Any*) = macro apply_impl
//
//}
//
//case class Demo (val y: Any, val args: List[Any]) 