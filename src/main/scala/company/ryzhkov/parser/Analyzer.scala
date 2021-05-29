package company.ryzhkov.parser

import scala.collection.mutable.ArrayBuffer

class Analyzer {
  import company.ryzhkov.Tree._
  import company.ryzhkov.parser.Tokenizer._

  private var treePoint: StructuredTree = _

  def analyze(tokens: ArrayBuffer[Token]): StructuredTree = {
    tokens.foreach {
      case LeftSquareBrace      => processLeftSquareBrace()
      case RightSquareBrace     => processRightBrace()
      case LeftCurlyBrace       => processLeftCurlyBrace()
      case RightCurlyBrace      => processRightBrace()
      case StringLiteral(value) => treePoint.addElement(StringLeaf(value))
      case NumberLiteral(value) => treePoint.addElement(NumberLeaf(value.toInt))
      case _                    => ignore()
    }
    while (treePoint.parent.isDefined) {
      treePoint = treePoint.parent.get
    }
    treePoint
  }

  private def ignore(): Unit = ()

  private def processLeftSquareBrace(): Unit = {
    if (treePoint == null) {
      treePoint = new ListLeaf()
    } else {
      val deeperList = new ListLeaf()
      treePoint.addElement(deeperList)
      deeperList.setParent(treePoint)
      treePoint = deeperList
    }
  }

  private def processLeftCurlyBrace(): Unit = {
    if (treePoint == null) {
      treePoint = new ObjectLeaf()
    } else {
      val deeperObject = new ObjectLeaf()
      treePoint.addElement(deeperObject)
      deeperObject.setParent(treePoint)
      treePoint = deeperObject
    }
  }

  private def processRightBrace(): Unit = {
    val parent = treePoint.parent
    if (parent.isDefined) treePoint = parent.get
  }
}
