trait Printable[A] {
  self =>

  def format: A => String
  def print: A => Unit = a => println(format(a))

  def contramap[B](func: B => A): Printable[B] = {
    new Printable[B] {
      def format: B => String = {
        b => self.format(func(b))
      }
    }
  }
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

  implicit val boolPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format: Boolean => String = x => if (x) "yes" else "no"
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = printable.print(value)
  }
}

