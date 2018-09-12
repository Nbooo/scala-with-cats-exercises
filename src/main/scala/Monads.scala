import cats.{Eval, Id, Monad}
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.language.higherKinds

trait MyMonad[F[_]] {
  def pure[T](a: T): F[T]

  def flatMap[A, B](a: A)(f: A => F[B]): F[B]

  def map[A, B](a: A)(f: A => B): F[B] = pure(f(a))

}

object Monads {
  def sumSquared[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = for {
    x <- a
    y <- b
  } yield x*x + y*y


  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](a: A)(f: A => Id[B]): Id[B] = f(a)

  def map[A, B](a: A)(f: A => B): Id[B] = f(a)

  def stackSafeFactorial: BigInt => Eval[BigInt] =
    num =>
      if (num == 1) Eval.now(num)
      else Eval.defer(stackSafeFactorial(num - 1).map(_ * num))


  def stackSafeFoldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
      case h::t => Eval.defer(fn(h, stackSafeFoldRight(t, acc)(fn)))
      case Nil  => acc
    }
}
