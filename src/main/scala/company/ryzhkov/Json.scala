package company.ryzhkov

sealed trait Json

object Json {
  case class JsonObject(list: List[(String, Json)]) extends Json
  case class JsonArray(list: List[Json]) extends Json
  case class JsonString(value: String) extends Json
  case class JsonNumber(value: Int) extends Json
}
