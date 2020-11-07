package calculators {
  object Calculators {

    type Operator = (Int, Int) => Int

    object Operator {
      val operators: Map[String, Operator] = Map(
        "+" -> { _ + _ },
        "-" -> { _ - _ },
        "*" -> { _ * _ },
        "/" -> { _ / _ })
      val operatorTr = operators map (_ swap)
      def unapply(token: String): Option[Operator] = {
        operators.get(token)
      }
    }

    object Number {
      def unapply(token: String): Option[Int] = try {
        Some(token.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    }

    sealed trait Expression
    case class NumberExpression(value: Int) extends Expression
    case class OperationExpression(lhs: Expression, rhs: Expression, op: Operator) extends Expression

    def step(list: List[Expression], token: String): List[Expression] =
      token match {
        case Number(number) => NumberExpression(number) :: list
        case Operator(op) => list match {
          case rhs :: lhs :: rest => OperationExpression(lhs, rhs, op) :: rest
          case _                  => throw new IllegalArgumentException("not enough operands");
        }
        case _ => throw new IllegalArgumentException("invalid token" + token);
      }

    def parse(expression: String): Expression = {
      val tokens = expression.split(" ")
      val stack = tokens.foldLeft(List.empty[Expression]) { step }
      stack.head
    }

    def calculate(expression: Expression): Int = expression match {
      case NumberExpression(value)           => value
      case OperationExpression(lhs, rhs, op) => op(calculate(lhs), calculate(rhs))
    }

    def toInfix(expression: Expression): String = {
      expression match {
        case NumberExpression(value)           => value.toString
        case OperationExpression(rhs, lhs, op) => s"(${toInfix(lhs) + Operator.operatorTr(op) + toInfix(rhs)})"
      }
    }

    def main(args: Array[String]): Unit = {
      if (args.length != 1) {
        throw new IllegalArgumentException("Usage: Calculator <expression> " + args.length)
      } else {
        val expression = parse(args(0))
        println(s"${toInfix(expression)} = ${calculate(expression)}")
      }
    }
  }
}
