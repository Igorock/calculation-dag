package com.dziuba.provision

import com.dziuba.provision.computeengine._
import com.dziuba.provision.computeengine.node.{ExpressionNode, Hold, Multiply, Sub, Sum, ValueNode}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec}

class CalculationEngineTest extends FlatSpec with MockitoSugar with BeforeAndAfter {

  it should "return calculated nodes" in {
    val test1 = ValueNode("test1", 3.0)
    val test2 = ValueNode("test2", 4.0)
    val test3 = ExpressionNode("test3", Multiply(test1, test2))
    val test4 = ExpressionNode("test4", Multiply(test3, Sum(test1, test2)))
    val test5 = ExpressionNode("test5", Sub(test4, test1, test2))
    val total = ExpressionNode("total", Sum(test5, test4))
    val test6 = ValueNode("test6", 5.0)
    val test7 = ExpressionNode("test7", Sum(test6))
    val root = ExpressionNode("root", Hold(test7, total))

    val rootNode = new CalculationEngine().computeGraph(root)
    assert(rootNode.isRoot)
    assertResult("(test7), (total)")(rootNode.formula)
    val totalNode = rootNode.findByName("total").get
    assertResult("total")(totalNode.name)
    assertResult(161)(totalNode.output)
    assertResult("((test5) + (test4))")(totalNode.formula)
    assertResult("(77.00 + 84.00)")(totalNode.valueFormula)
    assertResult(2)(totalNode.children.size)
    assertResult(Seq(77, 84))(totalNode.input)

    val test4Option = rootNode.findByName("test4")
    assert(test4Option.isDefined)
    assert(!test4Option.get.isRoot)
    assertResult("test4")(test4Option.get.name)
    assertResult(84)(test4Option.get.output)
    assertResult("(test3) * ((test1) + (test2))")(test4Option.get.formula)
    assertResult("12.00 * (3.00 + 4.00)")(test4Option.get.valueFormula)

    val test5Option = rootNode.findByName("test5")
    assertResult(77)(test5Option.get.output)
    assertResult("((test4) - (test1) - (test2))")(test5Option.get.formula)

    val test7Option = rootNode.findByName("test7")
    assertResult(5)(test7Option.get.output)
    assertResult("((test6))")(test7Option.get.formula)
    assertResult("(5.00)")(test7Option.get.valueFormula)
  }

}
