package company.ryzhkov

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object Program4 extends App {

  val str = """{"foo":5,"bar":"some","car":"BMW","flag":true,"obj":{"foo1":10}}"""
  //  val str = """{"foo":5}"""

  val result = parse(str)

  println(result)

  sealed trait ParsingState

  object ParsingState {
    case object StartJson  extends ParsingState
    case object StartName  extends ParsingState
    case object StartValue extends ParsingState
//    case object StartInnerJson extends ParsingState
  }

  sealed trait JSONValue

  object JSONValue {
    case class JSONInt(value: Int) extends JSONValue
    case class JSONString(value: String) extends JSONValue
    case class JSONObject(map: Map[String, JSONValue]) extends JSONValue
    case class JSONBoolean(value: Boolean) extends JSONValue
    case object JSONNull extends JSONValue
  }

  def parse(str: String): JSONValue.JSONObject = {
    val stack: mutable.Stack[ParsingState] = mutable.Stack.empty
    val key: mutable.Stack[Char]           = mutable.Stack.empty
    val value: mutable.Stack[Char]         = mutable.Stack.empty

    var json: JSONValue.JSONObject = JSONValue.JSONObject(map = Map.empty)

    str.toCharArray.foreach { ch =>
      if (stack.nonEmpty) {
        stack.head match {
          case ParsingState.StartJson  =>
          case ParsingState.StartName  =>
            if (ch != '"') key.push(ch)

          case ParsingState.StartValue =>
            value.push(ch)

//          case ParsingState.StartInnerJson =>
//            value.push(ch)
        }

        println(s"${stack.head} == $ch")
      }

      ch match {
        case '{' => processOpenCurlyBrace()
        case '"' => processQuotationMark()
        case ':' => processColon()
        case ',' => processCommaOrClosedCurlyBrace()
        case '}' => processCommaOrClosedCurlyBrace()
        case _   =>
      }
    }
    import JSONValue._

    def processOpenCurlyBrace(): Unit = {
      stack.push(ParsingState.StartJson)
//      if (stack.isEmpty) {
//        stack.push(ParsingState.StartJson)
//      } else {
//        stack.push(ParsingState.StartInnerJson)
//      }
    }

    def processQuotationMark(): Unit =
      stack.head match {
        case ParsingState.StartJson  => stack.push(ParsingState.StartName)
        case ParsingState.StartName  => stack.pop()
        case ParsingState.StartValue =>
      }

    def processColon(): Unit =
      stack.head match {
        case ParsingState.StartJson  => stack.push(ParsingState.StartValue)
        case ParsingState.StartName  =>
        case ParsingState.StartValue =>
      }

    def processCommaOrClosedCurlyBrace(): Unit =
      stack.head match {
        case ParsingState.StartJson  =>
        case ParsingState.StartName  =>
        case ParsingState.StartValue =>
          val jsonValue: JSONValue = processValue(value)
          val jsonName             = new String(key.reverse.toArray)

          println(jsonName -> jsonValue)
          json = json.copy(map = json.map + (jsonName -> jsonValue))
          key.clear()
          value.clear()
          stack.pop()
      }

    def processValue(value: mutable.Stack[Char]): JSONValue = {
      value.pop()
      val pattern = new String(value.reverse.toArray)

      val res = Try(pattern.toInt).toOption

      println("=======")
      println(pattern)
      println("=======")

      res match {
        case None        =>
          pattern match {
            case "true"  => JSONBoolean(true)
            case "false" => JSONBoolean(false)
            case _       =>
              val head = value.head
              val last = value.last
              (head, last) match {
                case ('"', '"') => JSONString(pattern.substring(1, pattern.length - 1))
                case ('}', '{') =>
                  println("BINGO")
                  parse(pattern.substring(1, pattern.length - 1))
                case _          =>
                  println("BINGO")
                  parse(pattern.substring(1, pattern.length - 1))
//                  throw new Exception("FAIL!!!")

              }
          }
        case Some(value) => JSONInt(value)
      }
    }
//    println(json.map)
    json
  }
}
