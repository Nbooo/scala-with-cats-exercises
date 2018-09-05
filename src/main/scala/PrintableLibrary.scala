trait Printable[A] {
  def format: A => String
  def print: A => Unit = a => println(format(a))
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(printable.format(value))
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format: String => String = identity
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format: Int => String = _.toString
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = printable.print(value)
  }
}

