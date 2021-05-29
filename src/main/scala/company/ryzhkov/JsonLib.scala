package company.ryzhkov

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Tree {
  protected val elements: ArrayBuffer[Tree] = ArrayBuffer.empty
  protected var parent: Tree                = _

  def getElements: ArrayBuffer[Tree] = elements
  def addElement(element: Tree): Unit = elements += element

  def getParent: Option[Tree] = Option(parent)
  def setParent(newParent: Tree): Unit = parent = newParent
}

object Tree {
  def notSupported: Nothing = throw new Exception("Not supported")
}

class NumberLeaf(val value: Int) extends Tree {
  override def getElements: ArrayBuffer[Tree] = ???
  override def addElement(element: Tree): Unit = ???
  override def getParent: Option[Tree] = ???
  override def setParent(newParent: Tree): Unit = ???

  override def toString = s"NumberLeaf($value)"
}

class StringLeaf(val value: String) extends Tree {
  override def getElements: ArrayBuffer[Tree] = ???
  override def addElement(element: Tree): Unit = ???
  override def getParent: Option[Tree] = ???
  override def setParent(newParent: Tree): Unit = ???

  override def toString = s"StringLeaf($value)"
}

class BooleanLeaf(val value: Boolean) extends Tree {
  override def getElements: ArrayBuffer[Tree] = ???
  override def addElement(element: Tree): Unit = ???
  override def getParent: Option[Tree] = ???
  override def setParent(newParent: Tree): Unit = ???

  override def toString = s"BooleanLeaf($value)"
}

class ListLeaf extends Tree {
  override def toString = s"ListLeaf($elements)"
}

class ObjectLeaf extends Tree {
  override def toString = s"ObjectLeaf($elements)"
}

object NullLeaf extends Tree

object JsonLib extends App {
//  val str = """["12","2","3", ["a","b"], {"car":"BMW"}, "abc"]"""
  val str = """{"foo":"bar","nested":{"bazz":"1","buzz":"2"},"car":"bmw","list":[["1","2","3"],["a","b"]]}"""
//  val str = """["12","2","3"]"""

//  val xxx = ArrayBuffer(1,2,3)
//  xxx += 4
//  println(xxx)

  sealed trait Token
  case object LeftCurlyBrace   extends Token
  case object RightCurlyBrace  extends Token
  case object LeftSquareBrace  extends Token
  case object RightSquareBrace extends Token
  case object QuotationMark    extends Token
  case class StringLiteral(value: String) extends Token
  case object Colon            extends Token
  case object Comma            extends Token
  case object TrueSign         extends Token
  case object FalseSign        extends Token
  case class NumberLiteral(value: Int) extends Token

  val tokens: ArrayBuffer[Token] = ArrayBuffer.empty

  var numberLiteral: mutable.Stack[Char] = mutable.Stack.empty
  var stringLiteral: ArrayBuffer[Char]   = ArrayBuffer.empty

  var isNumber = false
  var isString = false

  str.toCharArray.foreach { ch =>
    println("start")

    ch match {
      case '{' => tokens += LeftCurlyBrace

      case '}' => tokens += RightCurlyBrace

      case '[' => tokens += LeftSquareBrace

      case ']' => tokens += RightSquareBrace

      case '"' =>
        if (isString) {
          tokens += StringLiteral(new String(stringLiteral.toArray))
          stringLiteral.clear()
        }
        isString = !isString

      case ',' => tokens += Comma

      case x => if (isString) stringLiteral += x

      case _ =>
    }
  }

  println(tokens)

  println("==========")

  var treePoint: Tree = NullLeaf

  tokens.foreach { token =>
    println("start!!!")

    token match {
      case LeftSquareBrace      =>
        if (treePoint == NullLeaf) {
          treePoint = new ListLeaf()
        } else {
          val deeperList = new ListLeaf()
          treePoint.addElement(deeperList)
          deeperList.setParent(treePoint)
          treePoint = deeperList
        }

      case RightSquareBrace     =>
        val parent = treePoint.getParent
        parent match {
          case None        => ()
          case Some(value) =>
            treePoint = value
        }

      case LeftCurlyBrace       =>
        treePoint match {
          case NullLeaf => treePoint = new ObjectLeaf()
          case _        =>
            val deeperObject = new ObjectLeaf()
            treePoint.addElement(deeperObject)
            deeperObject.setParent(treePoint)
            treePoint = deeperObject
        }

      case RightCurlyBrace      =>
        val parent = treePoint.getParent
        parent match {
          case None        =>
          case Some(value) => treePoint = value
        }

      case QuotationMark        =>
      case StringLiteral(value) =>
        treePoint.addElement(new StringLeaf(value))

      case Colon                =>
      case Comma                =>
      case TrueSign             =>
      case FalseSign            =>
      case NumberLiteral(value) =>
    }
  }

  import scala.util.control.Breaks._

  while (treePoint.getParent.isDefined) {
    println(s"R: $treePoint")
    treePoint = treePoint.getParent.get
  }

  println(s"Result: $treePoint")
}
