package com.dziuba.provision

import com.dziuba.provision.computeengine.node.CalculatedNode
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec}

class CalculatedNodeTest extends FlatSpec with MockitoSugar with BeforeAndAfter {

  it should "test computed node initialization" in {
    val step1 = CalculatedNode("name1", 1.0)
    val step2 = CalculatedNode("name2", 2.0, children = Seq(step1))

    assertResult(step1)(step1.findByName("name1").get)
    assertResult(step2)(step2.findByName("name2").get)
    assertResult(step1)(step2.findByName("name1").get)
    assertResult(Seq(
      CalculatedNode("name1", 1.0),
      CalculatedNode("name2", 2.0, children = List(CalculatedNode("name1", 1.0))))
    )(step2.toList)
  }

}
