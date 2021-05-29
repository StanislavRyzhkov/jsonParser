package company.ryzhkov

import scala.collection.mutable

// todo сделать классы в стиле java!!!
object Pr7 extends App {
//  val str = """["12","2","3",["qwe","asd"]]"""
  val str = """["12","2","3", ["a","b"]]"""

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

  trait JValue {
    private[ryzhkov] def parent: Option[JValue] =
      this match {
        case jList: JList     => jList.parent
        case jObject: JObject => jObject.parent
        case _                => None
      }

    def addToList(jValue: JValue): JList = {
      this match {
        case jList: JList =>
          val newJList = jList.copy(values = jValue :: jList.values)

          if (jList.parent.isDefined) {
            newJList.renewParent(jList.parent.get)
          }

          newJList

        case _            => throw new Exception("!!!")
      }
    }

    private[ryzhkov] def renewParent(jValue: JValue): Unit = {
      this match {
        case jList: JList     => jList.renewParent(jValue)
        case jObject: JObject => jObject.renewParent(jValue)
        case _                => throw new Exception("BAR!!!")
      }
    }
  }
  case class JInt(value: Int) extends JValue

  case class JString(value: String) extends JValue

  case class JObject(map: Map[String, JValue] = Map()) extends JValue {
    private var _parent: JValue = _

    private[ryzhkov] override def parent: Option[JValue] = Option(_parent)

    private[ryzhkov] override def renewParent(jValue: JValue): Unit = {
      _parent = jValue
    }
  }

  case class JBoolean(value: Boolean) extends JValue

  case class JList(values: List[JValue] = List()) extends JValue {
    private var _parent: JValue = _

    private[ryzhkov] override def parent: Option[JValue] = Option(_parent)

    private[ryzhkov] override def renewParent(jValue: JValue): Unit = {
      _parent = jValue
    }
  }

  case object JNull extends JValue

  var list: List[Token] = Nil

  var numberLiteral: mutable.Stack[Char] = mutable.Stack.empty
  var stringLiteral: mutable.Stack[Char] = mutable.Stack.empty

  var isNumber = false
  var isString = false

  str.toCharArray.foreach { ch =>
    println("start")

    ch match {
      case '{' => list = LeftCurlyBrace :: list

      case '[' => list = LeftSquareBrace :: list

      case ']' => list = RightSquareBrace :: list

      case '"' =>
        if (isString) {
          list = StringLiteral(new String(stringLiteral.reverse.toArray)) :: list
          stringLiteral.clear()
        }
        isString = !isString

      case ',' => list = Comma :: list

      case x =>
        if (isString) stringLiteral.push(x)

      case _ =>
    }
  }

  list = list.reverse
  println(list)

  println("==========")

  var treePoint: JValue = JNull

  list.foreach { token =>
    println("start!!!")

    token match {
      case LeftSquareBrace      =>
        if (treePoint == JNull) {
          treePoint = JList()
        } else {
          println("YYYYYYYYYYYYY")
//          val list = JList(values = Nil, parent = Some(treePoint))
          val deeperList = JList(values = Nil)
          println(s"deeper; $deeperList")
          println(s"deeper; ${deeperList.parent}")
          treePoint = treePoint.addToList(deeperList)
          println(s"tree: $treePoint")
          println(s"tree: ${treePoint.parent}")
          deeperList.renewParent(treePoint)
          println(s"deeper; $deeperList")
          println(s"deeper; ${deeperList.parent}")
          treePoint = deeperList
          println(s"tree: $treePoint")
          println(s"tree: ${treePoint.parent}")
        }

      case RightCurlyBrace      =>
      case LeftSquareBrace      =>
      case RightSquareBrace     =>
      case QuotationMark        =>
      case StringLiteral(value) =>
        println(s"literal: $value")
        treePoint = treePoint.addToList(JString(value))
        println(s"tree: $treePoint")
        println(s"tree.parent: ${treePoint.parent}")

      case Colon                =>
      case Comma                =>
      case TrueSign             =>
      case FalseSign            =>
      case NumberLiteral(value) =>
    }
  }

  import scala.util.control.Breaks._

  while (treePoint.parent.isDefined) {
    println(s"R: $treePoint")
    treePoint = treePoint.parent.get
  }

  println(s"Result: $treePoint")
}
