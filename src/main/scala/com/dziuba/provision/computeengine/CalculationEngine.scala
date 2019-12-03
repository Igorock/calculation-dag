package com.dziuba.provision.computeengine

import com.dziuba.provision.computeengine.node.{CalculatedNode, Divide, Expression, ExpressionNode, Hold, Multiply, Sub, Sum, ValueNode}

class CalculationEngine {

  def computeGraph(node: Expression, isRoot: Boolean = true): CalculatedNode = node match {
    case ValueNode(field, value, _) => CalculatedNode(field, value)
    case ExpressionNode(field, expression, _) =>
      CalculatedNode(
        field,
        compute(field)(expression),
        buildFormula(expression),
        buildChildren(expression, Seq.empty).filter(c => c != Sum(List())).map(computeGraph(_, isRoot = false)),
        isRoot
      )
  }

  private def buildChildren(expression: Expression, nodes: Seq[Expression]): Seq[Expression] =
    expression.expressions match {
      case _ :: _ => expression.expressions.flatMap(e => buildChildren(e, nodes))
      case _ => nodes :+ expression
    }

  private def compute(f: String)(e: Expression): Double = e match {
    case ValueNode(_, value, _) => value
    case Sum(expressions) => expressions.map(compute(f)).sum
    case Multiply(expressions) => expressions.map(compute(f)).product
    case Sub(expressions) => expressions.tail.map(compute(f)).foldLeft(compute(f)(expressions.head))(_ - _)
    case Divide(expressions) => divide(f, expressions)
    case Hold(_) => 0
    case ExpressionNode(_, expression, _) => compute(f)(expression)
  }

  private def divide(f: String, expressions: Seq[Expression]) =
    compute(f)(expressions.head) / compute(f)(expressions(1))

  private def buildFormula(expression: Expression): String = expression match {
    case ValueNode(name, _, _) => s"($name)"
    case ExpressionNode(name, _, _) => s"($name)"
    case Sum(expressions) => "(" + expressions.map(buildFormula).mkString(" + ") + ")"
    case Multiply(expressions) => expressions.map(buildFormula).mkString(" * ")
    case Sub(expressions) => "(" + expressions.map(buildFormula).mkString(" - ") + ")"
    case Divide(expressions) => buildFormula(expressions.head) + " / " + buildFormula(expressions(1))
    case Hold(expressions) => expressions.map(buildFormula).mkString(", ")
  }

}

