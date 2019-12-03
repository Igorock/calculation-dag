package com.dziuba.provision.computeengine.node

import scala.annotation.tailrec

case class CalculatedNode(name: String,
                          output: Double,
                          formula: String = "",
                          valueFormula: String = "",
                          children: Seq[CalculatedNode] = Seq.empty,
                          input: Seq[Double] = Seq.empty,
                          isRoot: Boolean = false) {

  def findByName(stepName: String): Option[CalculatedNode] =
    findAmongChildren(List(this), head => head.name == stepName)

  @tailrec
  private def findAmongChildren(list: List[CalculatedNode], f: CalculatedNode => Boolean): Option[CalculatedNode] = {
    list match {
      case Nil => None
      case head :: tail =>
        if (f(head)) Some(head)
        else findAmongChildren(tail ++ head.children, f)
    }
  }

  def toList: Seq[CalculatedNode] = {
    (flatStep(children, Seq.empty) :+ this).distinct
  }

  private def flatStep(children: Seq[CalculatedNode], acc: Seq[CalculatedNode]): Seq[CalculatedNode] = {
    children.flatMap(s => s.children match {
      case _::_ => flatStep(s.children, acc) :+ s
      case _ => acc :+ s
    })
  }

  override def toString: String =
    if (children.isEmpty)
      s"(${name}, $output)"
    else
      s"(${name}, $output, $formula, $valueFormula, ${children.map(_.name)}, $input${if (isRoot) ", root" else ""})\n" + children.mkString("\n")
}

object CalculatedNode {
  def apply(name: String, output: Double, formula: String, children: Seq[CalculatedNode], isRoot: Boolean): CalculatedNode =
    apply(name, output, formula, buildValueFormula(formula, children), children, children.map(_.output), isRoot)

  private def buildValueFormula(formula: String, children: Seq[CalculatedNode]): String =
    children.map(c => (c.name, format(c.output)))
      .foldLeft(formula)((a, b) => a.replaceAllLiterally("(" + b._1 + ")", b._2))

  private def format(value: Double): String =
    BigDecimal(value).setScale(2, BigDecimal.RoundingMode.CEILING).toString()

}