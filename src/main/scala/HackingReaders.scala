import cats.data.Reader
import cats.syntax.applicative._
object HackingReaders {
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).fold(false)(_ == password))

  def checkLogin(userId: Int,  password: String): DbReader[Boolean] =
    for {
      username  <- findUsername(userId)
      valid     <- username.fold(false.pure[DbReader])(checkPassword(_, password))
    } yield valid
}
