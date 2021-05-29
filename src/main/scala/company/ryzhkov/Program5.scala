package company.ryzhkov

import scala.collection.mutable

object Program5 extends App {

  val str = """[12,2,3]"""

  sealed trait Stage

  object Stage {
    case object Start               extends Stage
    case object ParseList           extends Stage
    case object ParseListElemNumber extends Stage
  }

  sealed trait JSONValue {
    def toJSONList: JSONValue.JSONList =
      this match {
        case v @ JSONValue.JSONList(_) => v
        case _                         => throw new Exception("CAST!!!")
      }
  }

  object JSONValue {
    case class JSONInt(value: Int) extends JSONValue
    case class JSONString(value: String) extends JSONValue
    case class JSONObject(map: Map[String, JSONValue] = Map()) extends JSONValue
    case class JSONBoolean(value: Boolean) extends JSONValue
    case class JSONList(values: List[JSONValue] = List()) extends JSONValue
    case object JSONNull extends JSONValue
  }

  var initState: JSONValue    = JSONValue.JSONNull
  var currentState: JSONValue = JSONValue.JSONNull
//  var stage: Stage = Stage.Start

  val stages: mutable.Stack[Stage] = mutable.Stack(Stage.Start)

  var listElem: mutable.Stack[Char] = mutable.Stack.empty

  str.toCharArray.foreach { ch =>
    println("start")

//    ch match {
//      case '[' =>
//        if (stages.head == Stage.Start) {
//          initState = JSONValue.JSONList()
//          currentState = initState
//        }
//
//        stages.push(Stage.ParseList)
//
//      case x if x >= 49 && x <= 57 =>
//        if (stages.head == Stage.ParseList || stages.head == Stage.ParseListElemNumber) {
//          stages = Stage.ParseListElemNumber
//          listElem.push(ch)
//        }
//
//      case x if x == ',' || x ==']' =>
//        if (stage == Stage.ParseListElemNumber) {
//          stage = Stage.ParseList
//          val number = new String(listElem.reverse.toArray).toInt
//          currentState = currentState.toJSONList.copy(values = JSONValue.JSONInt(number) :: currentState.toJSONList.values)
//          listElem.clear()
//        }
//
//      case '"' =>
//        if (stage == Stage.ParseList) {
//
//        }
//
//      case _ =>
//    }
  }

  println(initState)
  println(currentState)
}
