import cats.effect._, cats._, cats.implicits._, cats.syntax._
import atto._, Atto._

object Main {

  sealed trait Expression[T]
  case class Number(value: Int) extends Expression[Int]
  case class Bool(value: Boolean) extends Expression[Boolean]

  case class Add[A, B](a: A with Expression[Int], b: B with Expression[Int]) extends Expression[Int]
  case class AND[A, B](a: A with Expression[Boolean], b: B with Expression[Boolean]) extends Expression[Boolean]

  trait Evaluator[E, T] {
    def evaluate(e: E with Expression[T]): T
  }

  def eval[E, T](e: E with Expression[T])(implicit ev: Evaluator[E, T]): T = ev.evaluate(e)

  def eitherEvaluator[E, T, E2, T2](e: Either[E with Expression[T], E2 with Expression[T2]])(implicit ev: Evaluator[E, T], ev2: Evaluator[E2, T2]): Either[T, T2] = e.map(eval).leftMap(eval)

  implicit object NumberEvaluator extends Evaluator[Number, Int] {
    def evaluate(n: Number) = n.value
  }

  implicit object BoolEvaluator extends Evaluator[Bool, Boolean] {
    def evaluate(b: Bool) = b.value
  }

  implicit def AddEvaluator[A, B](implicit ae: Evaluator[A with Expression[Int], Int], be: Evaluator[B with Expression[Int], Int]): Evaluator[Add[A, B], Int] = new Evaluator[Add[A, B], Int] {
    def evaluate(a: Add[A, B] with Expression[Int]) = eval(a.a) + eval(a.b)
  }

  implicit def ANDEvaluator[A, B](implicit ae: Evaluator[A with Expression[Boolean], Boolean], be: Evaluator[B with Expression[Boolean], Boolean]): Evaluator[AND[A, B], Boolean] = new Evaluator[AND[A, B], Boolean] {
    def evaluate(a: AND[A, B] with Expression[Boolean]) = eval(a.a) && eval(a.b)
  }

  val ptrue = string("true") named "true" map(_ => Bool(true))
  val pfalse = string("false") named "false" map(_ => Bool(false))
  val number = int named "number" map(i => Number(i))

  val add = char('+') named "add"
  val and = string("&&") named "and"

  val boolp = (ptrue | pfalse)

  val addExpr: Parser[Add[Number, Number] with Expression[Int]] = for {
    a <- number
    _ <- add
    b <- number
  } yield (Add(a, b))

  val andExpr: Parser[AND[Bool, Bool] with Expression[Boolean]] = for {
    a <- boolp
    _ <- and
    b <- boolp
  } yield (AND(a, b))


  val program = (addExpr || andExpr)


  def main(args: Array[String]): Unit = {
    println(number.parse("2").done.map(eval))
    println(program.parse("2+2").done.map(eitherEvaluator))
    println(program.parse("true&&true").done.map(eitherEvaluator))
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
