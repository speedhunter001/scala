package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (key, value) <- namedExpressions
      signalValue = Signal(eval(value(), namedExpressions))
    } yield key -> signalValue
  }

  private def evalRef(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Ref(name) =>
        val evaluated = getReferenceExpr(name, references)
        // ugly but working way to check for circular references. Name got used so remove it from map ->
        // we eventually get Double.NaN for self or circular references.
        if (evaluated.toString contains expr.toString) Double.NaN else eval(evaluated, references - name)
      case _ => eval(expr, references)
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(_) => evalRef(expr, references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
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
