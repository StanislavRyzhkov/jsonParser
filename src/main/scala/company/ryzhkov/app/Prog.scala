package company.ryzhkov.app

import company.ryzhkov.codec.DeciderUtils.decode
import company.ryzhkov.parser.Parser
import company.ryzhkov.codec.Decoder
import company.ryzhkov.codec.DecoderOps._
import company.ryzhkov.codec.DecoderOps.genericDecoder
import shapeless.HNil
import shapeless.{::, HList, HNil}
import shapeless._
import shapeless.Generic

object Prog extends App {
//  val str = """{"foo":"bar","nested":{"bazz":"1","buzz":"2"},"car":"bmw","list":[["1","2","3"],["a","b"]]}"""
//  val str = """{"foo":"bar","buzz":{"1":"one","2":"two"}}"""
//  val str = """[[1,2,3],[4,5,6],[7,8,9,100]]"""

  val str = """{"foo":"bar","buzz":5,"h":"huy","bar":{"x":5,"y":7}}"""

  case class Foo(foo: String, buzz: Int, h: String, bar: Bar)
  case class Bar(x: Int, y: Int)

//  val xxx = "" :: 5 :: HNil

  implicit val g = Generic[Foo]
  val json       = Parser.parse(str)
  val result     = decode[Foo](json)

  println(result)

//  import shapeless._

//  val xxx = the[Decoder[Int]]
}
