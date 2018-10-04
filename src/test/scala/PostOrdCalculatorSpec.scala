import PostOrdCalculator._
import org.scalatest.{FlatSpec, Matchers}

class PostOrdCalculatorSpec extends FlatSpec with Matchers {
  "Calculator" should "compute a correct value" in {

    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("4")
      ans <- evalOne("+")
    } yield ans

    program.runA(Nil).value shouldBe 5

    val program2 = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      _ <- evalOne("3")
      ans <- evalOne("*")
    } yield ans

    program2.runA(Nil).value shouldBe 9
  }

  "evalAll" should "compute correct result" in {
    val input = List("1", "2", "+")
    evalAll(input).runA(Nil).value shouldBe 3

    val input2 = List("1", "2", "+", "3", "*")
    evalAll(input2).runA(Nil).value shouldBe 9

    evalAll(Nil).runA(Nil).value shouldBe 0

    val program = for {
      a   <- evalAll(List("1", "2", "+"))
      b   <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    program.runA(Nil).value shouldBe 21
  }
}
