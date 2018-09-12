import org.scalatest.{FlatSpec, Matchers}
import cats.Id
import Monads._

class MonadSpec extends FlatSpec with Matchers {
  "sumSquared" should "be called with Id monad" in {
    sumSquared(3 : Id[Int], 3 : Id[Int]) shouldBe 18
  }

  "pure for Id" should "return an unchanged value" in {
    pure(100) shouldBe 100
    pure("hello") shouldBe "hello"
  }

  "map and flatMap for Id" should "return new value" in {
    flatMap(1)(_ + 1) shouldBe 2
    map(1)(_ + 1) shouldBe 2
    map(1)(_ + 1) shouldBe flatMap(1)(_ + 1)
  }

  "stackSafeFactorial" should "not fail with stackoverflow error" in {
    stackSafeFactorial(100000).value > 1 shouldBe true
  }
}
