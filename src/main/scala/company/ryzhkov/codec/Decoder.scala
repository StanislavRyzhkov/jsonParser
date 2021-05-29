package company.ryzhkov.codec

import company.ryzhkov.Json
import company.ryzhkov.Json.{JsonObject, JsonString}
import shapeless.Lazy

import shapeless.Generic

import scala.collection.immutable

trait Decoder[A] {
  def decode(json: Json): A
}

object Decoder {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  def instance[A](f: Json => A): Decoder[A] = (json: Json) => f(json)
}

object DecoderOps {
  implicit val stringDecoder: Decoder[String] = {
    case JsonString(value) => value
    case _                 => throw new Exception("Fail")
  }

  implicit val intDecoder: Decoder[Int] = {
    case Json.JsonNumber(value) => value
    case _                      => throw new Exception("Fail")
  }

  implicit def tuple2Decoder[A1, A2](implicit
    a1Decoder: Decoder[A1],
    a2Decoder: Decoder[A2]
  ): Decoder[(A1, A2)] = {
    case Json.JsonObject(List((_, v1), (_, v2))) => (a1Decoder.decode(v1), a2Decoder.decode(v2))
    case _                                       => throw new Exception("Fail")
  }

  implicit def listDecoder[A](implicit decoder: Decoder[A]): Decoder[List[A]] = {
    case Json.JsonArray(list) => list.map(decoder.decode)
    case _                    => throw new Exception("Fail")
  }

  import shapeless.{HList, ::, HNil}

  implicit val hNilDecoder: Decoder[HNil] = (_: Json) => HNil

  implicit def hListDecoder[H, T <: HList](implicit
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Decoder[T]
  ): Decoder[H :: T] = {
    case Json.JsonObject(list) =>
      val valueList = list.map { case (_, json) => json }
      valueList match {
        case immutable.::(head, next) =>
          val h: H = hDecoder.value.decode(head)
          val jo   = JsonObject(
            next.zipWithIndex
              .map(_.swap)
              .map { case (i, json) => (i.toString, json) }
          )
          val t: T = tDecoder.decode(jo)
          h :: t
        case immutable.::(head, Nil)  =>
          val h: H = hDecoder.value.decode(head)
          val t: T = tDecoder.decode(JsonObject(list = Nil))
          h :: t
      }

    case _                     => throw new Exception("Fail")
  }

  implicit def genericDecoder[A, R](implicit
    generic: Generic[A] { type Repr = R },
    decoder: Decoder[R]
  ): Decoder[A] = Decoder.instance(json => generic.from(decoder.decode(json)))
}

object DeciderUtils {
  def decode[A: Decoder](json: Json): A = {
    import shapeless._
    the[Decoder[A]].decode(json)
  }
}
