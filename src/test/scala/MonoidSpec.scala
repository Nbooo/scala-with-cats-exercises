import TruthAboutMonoid.Monoid
import TruthAboutMonoid.Monoid._
import org.scalatest.{Matchers, WordSpec}

class MonoidSpec extends WordSpec with Matchers {
  private def associativeLaw[A](x: A,  y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  private def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

  private def checkAssociativeLaw[A](l: List[A])(implicit m: Monoid[A]): Boolean = l match {
    case x::y::z::Nil => associativeLaw(x, y, z)
    case _            => false
  }

  "Monoid[Boolean]" should {

    "hold monoid laws for `true` as identity and `&&` as operation" in {
      implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        override def empty: Boolean = true
        override def combine(x: Boolean, y: Boolean): Boolean = x && y
      }

      val allTrues = List(true, true, true)
      val allFalses = List(false, false, false)
      val oneFalse = List(false, true, true).permutations.toList
      val twoFalses = List(false, false, true).permutations.toList

      val assocLaw = associativeLaw[Boolean](_: Boolean, _: Boolean, _: Boolean)(booleanMonoid)

      checkAssociativeLaw(allTrues) shouldBe true
      checkAssociativeLaw(allFalses) shouldBe true
      oneFalse.forall(checkAssociativeLaw[Boolean]) shouldBe true
      twoFalses.forall(checkAssociativeLaw[Boolean]) shouldBe true

      identityLaw(true) shouldBe true
      identityLaw(false) shouldBe true
    }

    "hold monoid laws for `false` as identity and `||` as operation" in {
      implicit  val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        override def empty: Boolean = false
        override def combine(x: Boolean, y: Boolean): Boolean = x || y
      }

      val allTrues = List(true, true, true)
      val allFalses = List(false, false, false)
      val oneFalse = List(false, true, true).permutations.toList
      val twoFalses = List(false, false, true).permutations.toList

      val assocLaw = associativeLaw[Boolean](_: Boolean, _: Boolean, _: Boolean)(booleanMonoid)

      checkAssociativeLaw(allTrues) shouldBe true
      checkAssociativeLaw(allFalses) shouldBe true
      oneFalse.forall(checkAssociativeLaw[Boolean]) shouldBe true
      twoFalses.forall(checkAssociativeLaw[Boolean]) shouldBe true

      identityLaw(true) shouldBe true
      identityLaw(false) shouldBe true
    }

  }

}
