package company.ryzhkov

import company.ryzhkov.parser.Tokenizer
import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {

  test("1") {
    val res = Tokenizer.createTokens("""
        |{"foo":555,"bar":"222","r3":[1,2,3]}
        |""".stripMargin)

    println(res)
  }

  test("2") {
    val res = Tokenizer.createTokens("""
                                       |[1,34,"asd","321",5,688]
                                       |""".stripMargin)

    println(res)
  }

  test("3") {
    val res = Tokenizer.createTokens("""[[1,2],[3,4],[5,6]]""")

    println(res)
  }

  test("4") {}

}
