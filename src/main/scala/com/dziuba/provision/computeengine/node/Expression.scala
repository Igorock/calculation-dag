package com.dziuba.provision.computeengine.node

trait Expression {
  def expressions: Seq[Expression]
}

trait Node extends Expression {
  def name: String
}

case class ExpressionNode(name: String, expression: Expression, expressions: Seq[Expression] = Seq.empty) extends Node
case class ValueNode(name: String, value: Double, expressions: Seq[Expression] = Seq.empty) extends Node

case class Sum(expressions: Seq[Expression]) extends Expression
case class Multiply(expressions: Seq[Expression]) extends Expression
case class Sub(expressions: Seq[Expression]) extends Expression
case class Divide(expressions: Seq[Expression]) extends Expression
case class Hold(expressions: Seq[Expression]) extends Expression

object Sum {
  def apply(a: Expression, b: Expression*): Sum = apply(Seq(a) ++ b)
}

object Multiply {
  def apply(a: Expression, b: Expression*): Multiply = apply(Seq(a) ++ b)
}

object Sub {
  def apply(a: Expression, b: Expression*): Sub = apply(Seq(a) ++ b)
}

object Divide {
  def apply(a: Expression, b: Expression): Divide = apply(Seq(a, b))
}

object Hold {
  def apply(a: Expression, b: Expression*): Hold = apply(Seq(a) ++ b)
}

object ExpressionNode {
  def apply(name: String, expression: Expression): Node = expression.expressions match {
    case _::_ => new ExpressionNode(name, expression)
    case _ => ValueNode(name, 0.0)
  }
}
