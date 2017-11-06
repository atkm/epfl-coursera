package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
    namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      namedExpressions.map {
        case (name, s) => name -> Signal {
          eval(s(), namedExpressions)
        }
      }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalRefWithPath(expr: Expr, path: List[Expr]): Double = expr match {
      case Ref(name) if path.contains(expr) => Double.NaN
      case Ref(name) => evalRefWithPath(getReferenceExpr(name, references), expr::path)
      case Literal(v) => v
      case Plus(a, b) => evalRefWithPath(a, path) + evalRefWithPath(b, path)
      case Minus(a, b) => evalRefWithPath(a, path) - evalRefWithPath(b, path)
      case Times(a, b) => evalRefWithPath(a, path) * evalRefWithPath(b, path)
      case Divide(a, b) => evalRefWithPath(a, path) / evalRefWithPath(b, path)
    }
    evalRefWithPath(expr, Nil)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
