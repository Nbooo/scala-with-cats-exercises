import cats._
import cats.syntax._
import cats.implicits._
import org.scalatest.{Matchers, WordSpec}

class PrintableLibrarySpec extends WordSpec with Matchers {
  "Printable" should {
    "format integers and strings" in {
      import PrintableInstances._
      val stringValue = "string"
      val intValue = 123

      Printable.format(stringValue) shouldBe stringValue
      Printable.format(intValue) shouldBe intValue.toString
    }

    "format a Cat into string correctly" in new PrintableCatTestScope { // xD that's cruel for sure.
      val cat: Cat = Cat("Taiva", 2, "black")
      val expected = s"${cat.name} is a ${cat.age} years-old ${cat.color} cat."

      Printable.format(cat) shouldBe expected
    }

    "format on an entity may be called via Ops" in new PrintableCatTestScope {
      import PrintableInstances._
      import PrintableSyntax._

      val intValue = 123
      intValue.format shouldBe intValue.toString

      val cat: Cat = Cat("Joe", 12, "brown")
      val expected = s"${cat.name} is a ${cat.age} years-old ${cat.color} cat."

      cat.format shouldBe expected
    }
  }

  "cats.Show" should {
    "be used instead of Printable[_]" in {
      val cat: Cat = Cat("Joe", 12, "brown")
      val expected = s"${cat.name} is a ${cat.age} years-old ${cat.color} cat."

      implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} years-old ${cat.color} cat.")

      cat.show shouldBe expected
    }
  }

}

final case class Cat(name: String, age: Int, color: String)

trait PrintableCatTestScope {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format: Cat => String =
      cat => s"${cat.name} is a ${cat.age} years-old ${cat.color} cat."
  }
}