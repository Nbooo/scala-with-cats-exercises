import cats.data.State
import cats.syntax.applicative._
import scala.util.Try

object PostOrdCalculator {
  sealed trait Symbol
  sealed trait BiOperation extends Symbol {
    def run: (Int, Int) => Int
  }

  case class Digit(value: Int) extends Symbol

  case object Plus extends BiOperation {
    override def run: (Int, Int) => Int = _ + _
  }

  case object Minus extends BiOperation {
    override def run: (Int, Int) => Int = _ - _
  }
  case object Mult extends BiOperation {
    override def run: (Int, Int) => Int = _ * _
  }

  case object Div extends BiOperation {
    override def run: (Int, Int) => Int = _ / _ // not safe obviously, but kinda ok for exercise
  }

  object Symbol {
    def apply(sym: String): Option[Symbol] = sym match {
      case "+"    => Some(Plus)
      case "-"    => Some(Minus)
      case "*"    => Some(Mult)
      case "/"    => Some(Div)
      case other  => Try(other.toInt).toOption.map(Digit.apply)
    }
  }

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] {
    stack => Symbol(sym) map {
      case Digit(value)     => (value::stack, 0)
      case op: BiOperation  => performOperation(stack, op)
    } getOrElse(stack, 0)
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input
      .foldLeft(0.pure[CalcState]) {
        (state, sym) => state.flatMap(_ => evalOne(sym))
      }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  private def performOperation: (List[Int], BiOperation) => (List[Int], Int) = (xs, op) => xs match {
    case a::b::tail =>
      val result = op.run(a, b)
      (result :: tail, result)
    case _ => (xs, 0) // can't perform an operation.
  }
}
