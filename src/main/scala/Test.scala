object ExpProblem1 {

  trait Exp
  trait Lit extends Exp
  trait BinOp extends Exp

  trait Eval[A] {
    def eval(x: A): String
  }

  object Eval {
    def apply[A](implicit x: Eval[A]): Eval[A] = x

    def eval[A: Eval](a: A) = Eval[A].eval(a)

    implicit class EvalOps[A: Eval](a: A) {
      def eval = Eval[A].eval(a)
    }

    //type class instances
    implicit def EvalInteger: Eval[Integer] = x => s"$x"
    implicit def EvalMultiply[A: Eval, B: Eval]: Eval[Multiply[A,B]] = x => Eval[A].eval(x.l) ++ "+" ++ Eval[B].eval(x.r)

    case class Integer(v: String) extends Lit
    case class Multiply[A: Eval, B: Eval](l: A, r: B) extends BinOp
  }
}
