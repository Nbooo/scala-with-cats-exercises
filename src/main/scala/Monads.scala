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

object CustomMonads {
  import Tree._

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    // 2^n complexity, n - depth of a tree.
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Branch(left, right)  => Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(a)              => f(a)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      ???
    }

    private def tailRec[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def unbox(tree: Tree[Either[A, B]]): Option[Tree[B]] = tree match {
        case Branch(left, right)  => ???
        case Leaf(value)          => value match {
          case Left(_)  => None
          case Right(b) => Some(Leaf(b))
        }
      }

      def mergeTuple(l: Option[Tree[B]], r: Option[Tree[B]]): Option[Tree[B]] = (l, r) match {
        case (Some(left), Some(right))  => Some(Branch(left, right))
        case _                          => None
      }

      f(a) match {
        case Branch(left, right) => mergeTuple(unbox(left), unbox(right)) match {
          case Some(tree) => tree
          case _          => tailRec(a)(f)
        }
      }
    }
  }
}

