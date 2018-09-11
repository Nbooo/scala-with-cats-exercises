trait Codec[A] {
  self =>

  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))

    def decode(value: String): B = dec(self.decode(value))
  }
}


object CodecSyntax {
  implicit class CodecEncOp[A](value: A) {
    def encode(implicit codec: Codec[A]): String = codec.encode(value)
  }

  implicit class CodecDecOp[A](value: String) {
    def decode(implicit codec: Codec[A]): A = codec.decode(value)
  }
}
