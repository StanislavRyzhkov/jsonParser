package company.ryzhkov

import scala.collection.mutable

object Program2 extends App {
  val str = """{"foo":5}"""

  val m = Map("1" -> "1")

  val m2 = m + ("2" -> "2")

//  case class Foo(foo: Int)

  sealed trait JSONValue
  case class JSONInt(value: Int) extends JSONValue
  case class JSONObject(map: Map[String, JSONValue]) extends JSONValue
  case object JSONNull extends JSONValue

  str.toCharArray foreach { ch =>
    println(s"${ch.toInt} = $ch")
  }

  val stack: mutable.Stack[Int]  = mutable.Stack.empty
  val name: mutable.Stack[Char]  = mutable.Stack.empty
  val value: mutable.Stack[Char] = mutable.Stack.empty

  var json: JSONObject = JSONObject(map = Map.empty)

  str.toCharArray.foreach { ch =>
    if (stack.nonEmpty) {
      if (stack.head == '"' && ch != '"') {
        name.push(ch)
      } else if (stack.head == ':' && ch != '}') {
        value.push(ch)
      }
    }

    ch match {
      case '{' =>
        stack.push('{')

      case '"' =>
        if (stack.head == '"') {
          stack.pop()
        } else {
          stack.push('"')
        }

      case ':' =>
        stack.push(':')

      case '}' =>
        val jsonValue: JSONValue = parseString(new String(value.reverse.toArray))
        val jsonName             = new String(name.reverse.toArray)

        json = json.copy(map = json.map + (jsonName -> jsonValue))

        stack.pop()
        if (stack.nonEmpty) stack.pop()

      case _   =>
    }
  }

  def parseString(s: String) = {
    JSONInt(s.toInt)
  }

  println(new String(name.reverse.toArray))
  println("=====")
  println(new String(value.reverse.toArray))

  println("!!!!!!!!!")
  println("!!!!!!!!!")
  println("!!!!!!!!!")
  println("!!!!!!!!!")
  println(json)
}
