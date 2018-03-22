import cats.effect._, cats._, cats.implicits._, cats.syntax._
import atto._, Atto._

object Main {

  sealed trait Expression[+T]
  case class Number(value: Int) extends Expression[Number] {
    def plus(n: Number): Number = Number(value + n.value)
  } 
  case class Bool(value: Boolean) extends Expression[Bool] {
    def and(b: Bool): Bool = Bool(value && b.value)
  }
  case class Str(value: String) extends Expression[Str] {
    def concat(s: Str): Str = Str(value + s.value)
  }
  case class MyUnit() extends Expression[MyUnit] {
    implicit val eval: Eval[MyUnit, MyUnit] = (u: WhichReturnsItself[MyUnit]) => u
  }

  case class Add[A, B, O](a: A & Expression[O], b: B & Expression[O]) extends Expression[O]
  case class AND[A, B, O](a: A & Expression[O], b: B & Expression[O]) extends Expression[O]

  type WhichReturns[A, T] = A & Expression[T]
  type WhichReturnsItself[T] = T WhichReturns T

  type Eval[E, T] = Evaluator[E WhichReturns T, T]

  trait Evaluator[E, T] {
    def evaluate(e: E WhichReturns T): T
  }

  def eval[E, T](e: E & Expression[T])(implicit ev: Evaluator[E, T]): T = ev.evaluate(e)

  def eitherEvaluator[E, T, E2, T2](e: Either[E WhichReturns T, E2 WhichReturns T2])(implicit ev: Evaluator[E, T], ev2: Evaluator[E2, T2]): Either[T, T2] = e.map(eval).leftMap(eval)

  def identityEvaluator[T](): Evaluator[T WhichReturns T, T] = (e: T WhichReturns T) => e

  implicit val numberIdentity: Evaluator[Number WhichReturns Number, Number] = identityEvaluator[Number]()
  implicit val boolIdentity: Evaluator[Bool WhichReturns Bool, Bool] = identityEvaluator[Bool]()
  implicit val strIdentity: Evaluator[Str WhichReturns Str, Str] = identityEvaluator[Str]()

  implicit def AddIntEvaluator[A, B](implicit ae: Eval[A, Number], be: Eval[B, Number]): Evaluator[Add[A, B, Number], Number] = new Evaluator[Add[A, B, Number], Number] {
    def evaluate(a: Add[A, B, Number] WhichReturns Number): Number = eval(a.a).plus(eval(a.b))
  }

  implicit def AddStringEvaluator[A, B](implicit ae: Eval[A, Str], be: Eval[B, Str]): Evaluator[Add[A, B, Str], Str] = (a: Add[A, B, Str] WhichReturns Str) => eval(a.a).concat(eval(a.b))

  implicit def ANDEvaluator[A, B](implicit ae: Eval[A, Bool], be: Eval[B, Bool]): Evaluator[AND[A, B, Bool], Bool] = (a: AND[A, B, Bool] WhichReturns Bool) => eval(a.a).and(eval(a.b))

  val ptrue: Parser[Bool WhichReturns Bool] = string("true") named "true" map(_ => Bool(true))
  val pfalse: Parser[Bool WhichReturns Bool] = string("false") named "false" map(_ => Bool(false))
  val number: Parser[Number WhichReturns Number] = int named "number" map(i => Number(i))

  val add = char('+') named "add"
  val and = string("&&") named "and"
  val quote = char('"') named "quote"

  val stringLiteral: Parser[Str WhichReturns Str] = for {
    _ <- quote
    str <- takeWhile(_!='"')
    _ <- quote
  } yield (Str(str))

  val boolp: Parser[Bool WhichReturns Bool] = (ptrue | pfalse)

  type Literal = Str | Number | Bool
  val literal: Parser[Literal] = stringLiteral.widen[Literal] | number.widen[Literal] | boolp.widen[Literal]

  val addExpr: Parser[Add[WhichReturnsItself[Number], WhichReturnsItself[Number], Number] WhichReturns Number] = for {
    a <- number 
    _ <- add
    b <- number
  } yield (Add(a, b))

  val andExpr: Parser[AND[Bool WhichReturns Bool, Bool WhichReturns Bool, Bool] WhichReturns Bool] = for {
    a <- boolp
    _ <- and
    b <- boolp
  } yield (AND(a, b))


  val program = (addExpr || andExpr)


  def main(args: Array[String]): Unit = {
    println(number.parse("2").done.map(eval))
    println(addExpr.parse("2+2").done.map(eval))
    println(andExpr.parse("true&&false").done.map(eval))
    println(andExpr.parse("true&&true").done.map(eval))
    println(program.parse("2+2").done.map(eitherEvaluator))
    println(program.parse("true&&true").done.map(eitherEvaluator))
    val f = program.parse("42+42").done.map(eitherEvaluator)
    f.map { 
      _ match {
        case Left(i) => println(i)
        case Right(b) => println(b)
      }
    }
    // literals
    println(List("\"hello\"", "12", "true").map(literal.parse).map(_.done))
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
