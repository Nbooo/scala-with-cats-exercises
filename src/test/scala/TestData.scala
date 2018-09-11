trait TestData {
  final case class Cat(name: String, age: Int, color: String)
  final case class Box[A](value: A)
}
