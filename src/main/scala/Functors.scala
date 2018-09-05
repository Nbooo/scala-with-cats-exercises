import cats.Functor
import cats.syntax.functor._

object Functors {
  def doMath[F[_]](start: F[Int])
                  (implicit func: Functor[F]): F[Int] =
    start.map(_ + 1 * 2)
}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  object ops {
    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v)      => Leaf(f(v))
      }
    }
  }
}
