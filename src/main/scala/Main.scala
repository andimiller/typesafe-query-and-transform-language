import cats.effect._, cats._, cats.implicits._, cats.syntax._
import atto._, Atto._

object Main {

  sealed trait Expression[T]
  case class Number(value: Int) extends Expression[Int]
  case class Bool(value: Boolean) extends Expression[Boolean]

  case class Add(a: Expression[Int], b: Expression[Int]) extends Expression[Int]
  case class AND(a: Expression[Boolean], b: Expression[Boolean]) extends Expression[Boolean]

  def eval[T](e: Expression[T]): T = e match {
    case Number(i) => i
    case Bool(b) => b
    case Add(a, b) => eval(a) + eval(b)
    case AND(a, b) => eval(a) && eval(b)
  }

  val ptrue = string("true") named "true" map(_ => Bool(true))
  val pfalse = string("false") named "false" map(_ => Bool(false))
  val number = int named "number" map(i => Number(i))

  val add = char('+') named "add"
  val and = string("&&") named "and"

  val boolp = (ptrue | pfalse)

  val addExpr: Parser[Expression[Int]] = for {
    a <- number
    _ <- add
    b <- number
  } yield (Add(a, b))

  val andExpr: Parser[Expression[Boolean]] = for {
    a <- boolp
    _ <- and
    b <- boolp
  } yield (AND(a, b))


  val program: Parser[Expression[Int | Boolean]] = (addExpr union andExpr).map(_.typemerge())

  def main(args: Array[String]): Unit = {
    println(program.parse("2+2").done.map(eval))
    println(program.parse("true&&true").done.map(eval))
  }

  implicit class DottyParser[A](a: Parser[A]) {
    def union[B](b: Parser[B]): Parser[A | B] = {
      (a || b).map(_.merge)
    }
  }

  implicit class MergeOps[M[_], A, B](m: M[A] | M[B]) {
    def typemerge(): M[A | B] = m.asInstanceOf[M[A | B]]
  }

}
