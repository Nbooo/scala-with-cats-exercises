import org.scalatest.{FlatSpec, Matchers}
import Functors._
import cats.instances.option._

class FunctorsSpec extends FlatSpec with Matchers {
  "doMath[F[_]]" should "compute a correct result for Option" in {
    doMath(Option(3)) shouldBe Some(5)
  }

  "Tree functor" should "work correctly" in {
    import Tree.ops._
    import cats.syntax.functor._

    val intTree: Tree[Int] = Branch(Leaf(1), Leaf(23))
    val stringTree: Tree[String] = Branch(Leaf("hello"), Leaf("goodbye"))

    intTree.map(_ * 2) shouldBe Branch(Leaf(2), Leaf(46))
    stringTree.map(_ + " world") shouldBe Branch(Leaf("hello world"), Leaf("goodbye world"))
  }
}
