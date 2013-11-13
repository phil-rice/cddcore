package org.cddcore.engine

class Maps {

}
object Maps {

  def addToList[K, V](map: Map[K, List[V]], key: K, value: V) = map + (key -> (value :: map.getOrElse(key, Nil)))

  def addToMapOfMapOfList[K1, K2, V](map: Map[K1, Map[K2, List[V]]], key1: K1, key2: K2, value: V) = {
    val map2: Map[K2, List[V]] = map.getOrElse(key1, Map())
    map + (key1 -> (map2 + (key2 -> (value :: map2.getOrElse(key2, List())))))
  }

  def walkSelfAndChildrenPaths[KV](map: Map[KV, List[KV]])(root: KV) = {
    def fn(root: KV): Iterable[List[KV]] ={
      val children = map.getOrElse(root, Nil)
      (List(List()) ++ children.flatMap(fn(_))).map(  root :: _)
    }
    fn(root)
  }

}