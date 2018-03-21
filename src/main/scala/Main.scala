import cats.effect._, cats._, cats.implicits._, cats.syntax._
import atto._, Atto._

object Main {

  sealed trait Expression[+T]
  case class Number(value: Int) extends Expression[Int]
  case class Bool(value: Boolean) extends Expression[Boolean]

  case class Add[A, B](a: A & Expression[Int], b: B & Expression[Int]) extends Expression[Int]
  case class AND[A, B](a: A & Expression[Boolean], b: B & Expression[Boolean]) extends Expression[Boolean]

  type WhichReturns[A, T] = A & Expression[T]

  type Eval[E, T] = Evaluator[E WhichReturns T, T]

  trait Evaluator[E, T] {
    def evaluate(e: E WhichReturns T): T
  }

  def eval[E, T](e: E & Expression[T])(implicit ev: Evaluator[E, T]): T = ev.evaluate(e)

  def eitherEvaluator[E, T, E2, T2](e: Either[E WhichReturns T, E2 WhichReturns T2])(implicit ev: Evaluator[E, T], ev2: Evaluator[E2, T2]): Either[T, T2] = e.map(eval).leftMap(eval)

  implicit object NumberEvaluator extends Evaluator[Number, Int] {
    def evaluate(n: Number) = n.value
  }

  implicit object BoolEvaluator extends Evaluator[Bool, Boolean] {
    def evaluate(b: Bool) = b.value
  }

  implicit def AddEvaluator[A, B](implicit ae: Eval[A, Int], be: Eval[B, Int]): Evaluator[Add[A, B], Int] = new Evaluator[Add[A, B], Int] {
    def evaluate(a: Add[A, B] & Expression[Int]) = eval(a.a) + eval(a.b)
  }

  implicit def ANDEvaluator[A, B](implicit ae: Eval[A, Boolean], be: Eval[B, Boolean]): Evaluator[AND[A, B], Boolean] = new Evaluator[AND[A, B], Boolean] {
    def evaluate(a: AND[A, B] & Expression[Boolean]) = eval(a.a) && eval(a.b)
  }

  val ptrue = string("true") named "true" map(_ => Bool(true))
  val pfalse = string("false") named "false" map(_ => Bool(false))
  val number = int named "number" map(i => Number(i))

  val add = char('+') named "add"
  val and = string("&&") named "and"

  val boolp = (ptrue | pfalse)

  val addExpr: Parser[Add[Number, Number] & Expression[Int]] = for {
    a <- number
    _ <- add
    b <- number
  } yield (Add(a, b))

  val andExpr: Parser[AND[Bool, Bool] & Expression[Boolean]] = for {
    a <- boolp
    _ <- and
    b <- boolp
  } yield (AND(a, b))


  val program = (addExpr || andExpr)


  def main(args: Array[String]): Unit = {
    println(number.parse("2").done.map(eval))
    println(program.parse("2+2").done.map(eitherEvaluator))
    println(program.parse("true&&true").done.map(eitherEvaluator))
    val f = program.parse("42+42").done.map(eitherEvaluator)
    f.map { 
      _ match {
        case Left(i) => println(i)
        case Right(b) => println(b)
      }
    }
  }

  implicit class DottyParser[A](a: Parser[A]) {
    def union[B](b: Parser[B]): Parser[A | B] = {
      (a || b).map(_.merge)
    }
  }

  implicit class MergeOps[M[_], A, B](m: M[A] | M[B]) {
    def typemerge(): M[A | B] = m.asInstanceOf[M[A | B]]
  }

  implicit class EitherMergeOps[A, B](e: Either[A, B]) {
    def mergeToIntersection: A | B = e.leftWiden[A | B].widen[A | B] match {
      case Left(a) => a
      case Right(b) => b
    }
  }

}
