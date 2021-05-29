package company.ryzhkov

import company.ryzhkov.Program3.JSONValue.{JSONBoolean, JSONInt, JSONString}

import scala.collection.mutable
import scala.util.Try

object Program3 extends App {
  val str = """{"foo":5,"bar":"some","car":"BMW","flag":true}"""
//  val str = """{"foo":5}"""

  sealed trait ParsingState

  object ParsingState {
    case object StartJson  extends ParsingState
    case object StartName  extends ParsingState
    case object StartValue extends ParsingState
  }

  sealed trait JSONValue

  object JSONValue {
    case class JSONInt(value: Int) extends JSONValue
    case class JSONString(value: String) extends JSONValue
    case class JSONObject(map: Map[String, JSONValue]) extends JSONValue
    case class JSONBoolean(value: Boolean) extends JSONValue
    case object JSONNull extends JSONValue
  }

  val stack: mutable.Stack[ParsingState] = mutable.Stack.empty
  val key: mutable.Stack[Char]           = mutable.Stack.empty
  val value: mutable.Stack[Char]         = mutable.Stack.empty

  var json: JSONValue.JSONObject = JSONValue.JSONObject(map = Map.empty)

  object StringPattern {
    def unapply(pattern: String) = {}
  }

  str.toCharArray.foreach { ch =>
    if (stack.nonEmpty) {
      stack.head match {
        case ParsingState.StartJson  =>
        case ParsingState.StartName  =>
          if (ch != '"') key.push(ch)

        case ParsingState.StartValue =>
          value.push(ch)
      }

      println(s"${stack.head} == $ch")
    }

    ch match {
      case '{' =>
        stack.push(ParsingState.StartJson)

      case '"' =>
        stack.head match {
          case ParsingState.StartJson  =>
            stack.push(ParsingState.StartName)

          case ParsingState.StartName  =>
            stack.pop()

          case ParsingState.StartValue =>
        }

      case ':' =>
        stack.head match {
          case ParsingState.StartJson  =>
            stack.push(ParsingState.StartValue)

          case ParsingState.StartName  =>
          case ParsingState.StartValue =>
        }

      case ',' =>
        stack.head match {
          case ParsingState.StartJson  =>
          case ParsingState.StartName  =>
          case ParsingState.StartValue =>
//            val jsonValue: JSONValue = JSONValue.JSONString(new String(value.reverse.toArray))
            val jsonValue: JSONValue = processValue(value)
            val jsonName             = new String(key.reverse.toArray)

            println(jsonName -> jsonValue)
            json = json.copy(map = json.map + (jsonName -> jsonValue))
            key.clear()
            value.clear()
            stack.pop()
        }

      case '}' =>
        stack.head match {
          case ParsingState.StartJson  =>
          case ParsingState.StartName  =>
          case ParsingState.StartValue =>
//            val jsonValue: JSONValue = JSONValue.JSONString(new String(value.reverse.toArray))
            val jsonValue = processValue(value)
            val jsonName  = new String(key.reverse.toArray)

            println(jsonName -> jsonValue)
            json = json.copy(map = json.map + (jsonName -> jsonValue))
            key.clear()
            value.clear()
            stack.pop()
        }

      case _   =>
    }
  }

  def processValue(value: mutable.Stack[Char]): JSONValue = {
    value.pop()
    val pattern = new String(value.reverse.toArray)

    val res = Try(pattern.toInt).toOption

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
              case _          => throw new Exception("FAIL!!!")

            }
        }
      case Some(value) => JSONInt(value)
    }
  }

  println(json.map)

}
