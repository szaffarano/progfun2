package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (n, s) => {
        if (!namedExpressions.contains(n)) (n, Signal(Double.NaN))
        else (n, Signal {
          eval(s(), namedExpressions)
        })
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def safeEval(evaluated: List[String], expr: Expr): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => {
          if (!references.contains(name)) Double.NaN
          else if (evaluated.contains(name)) Double.NaN
          else safeEval(name :: evaluated , references(name)())
        }
        case Plus(a, b) => safeEval(evaluated, a) + safeEval(evaluated, b)
        case Minus(a, b) => safeEval(evaluated, a) - safeEval(evaluated, b)
        case Times(a, b) => safeEval(evaluated, a) * safeEval(evaluated, b)
        case Divide(a, b) => {
          val divider = safeEval(evaluated, b)
          if (divider == 0) Double.NaN
          else safeEval(evaluated, a) / divider
        }
      }
    }

    safeEval(List(), expr)
  }


  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
