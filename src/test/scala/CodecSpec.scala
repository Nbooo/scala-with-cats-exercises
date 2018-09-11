import org.scalatest.{FlatSpec, Matchers}
import CodecSyntax._

class CodecSpec extends FlatSpec with Matchers with TestData {

  private implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  "Int and Boolean codecs" should "be constructed from given String codec" in {
    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
    implicit val boolCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

    10.encode shouldBe "10"
    false.encode shouldBe "false"
  }

  "Box[A] codec" should "be implemented and work" in {
    implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
      stringCodec.imap[Box[A]](maybeBox => Box(codec.decode(maybeBox)), box => codec.encode(box.value))

    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

    val intBox: Box[Int] = Box(123)

    intBox.encode shouldBe "123"
  }

}

