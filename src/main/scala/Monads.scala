import cats.data.Writer
import cats.{Eval, Id, Monad}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._
import scala.language.higherKinds
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

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

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"Factorial of $n is $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int]= {
    for {
      ans <-
        if (n == 0)
          1.pure[Logged]
        else
          slowly(factorialW(n - 1).map(_ * n))
      _   <- Vector(s"Factorial of $n is $ans").tell
    } yield ans
  }

  def main(args: Array[String]): Unit = {
    val res = Await.result(
      Future.sequence(
        Seq(
          Future(factorialW(15)),
          Future(factorialW(13)))),
      5.seconds)

    res.foreach(value => println(value.written))
  }
}



