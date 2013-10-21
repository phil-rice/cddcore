package org.cddcore.example.shoppingCart


case class SKU(name: String, price: Int)


object ShoppingCart



case class offer(name: String)

case class ShoppingCart (items: Map[SKU, Int] = Map()){
  def scan(item: SKU, count: Int = 1) = {
    val newValue = items.get(item) match {
      case Some(q) => q+ count
      case _ => count
    }
    ShoppingCart(items + (item -> newValue))
  }
}