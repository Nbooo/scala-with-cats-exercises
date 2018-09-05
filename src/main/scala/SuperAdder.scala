import TruthAboutMonoid.Monoid

trait SuperAdder {
  def add(items: List[Int]): Int = items.sum

  def add[K](items: List[K])(implicit monoid: Monoid[K]): K = items.foldLeft(monoid.empty)(monoid.combine)

  case class Order(totalCost: Double, quantity: Double)

  implicit val ordersMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(0, 0)

    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}
