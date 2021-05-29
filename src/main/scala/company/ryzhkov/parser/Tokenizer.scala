package company.ryzhkov.parser

import scala.collection.mutable.ArrayBuffer

object Tokenizer {
  sealed trait Token
  case object LeftCurlyBrace   extends Token
  case object RightCurlyBrace  extends Token
  case object LeftSquareBrace  extends Token
  case object RightSquareBrace extends Token
  case object QuotationMark    extends Token
  case object Colon            extends Token
  case object Comma            extends Token
  case object TrueSign         extends Token
  case object FalseSign        extends Token
  case class NumberLiteral(
    value: String
  )                            extends Token
  case class StringLiteral(
    value: String
  )                            extends Token

  def createTokens(str: String): ArrayBuffer[Token] = {
    val tokens: ArrayBuffer[Token]       = ArrayBuffer.empty
    var numberLiteral: ArrayBuffer[Char] = ArrayBuffer.empty
    var stringLiteral: ArrayBuffer[Char] = ArrayBuffer.empty

    var isNumber = false
    var isString = false

    str.toCharArray.foreach {
      case '{'                                  => tokens += LeftCurlyBrace
      case '}'                                  =>
        if (isNumber) {
          isNumber = !isNumber
          tokens += NumberLiteral(new String(numberLiteral.toArray))
          numberLiteral.clear()
        }
        tokens += RightCurlyBrace
      case '['                                  => tokens += LeftSquareBrace
      case ']'                                  =>
        if (isNumber) {
          isNumber = !isNumber
          tokens += NumberLiteral(new String(numberLiteral.toArray))
          numberLiteral.clear()
        }
        tokens += RightSquareBrace
      case '"'                                  =>
        if (isString) {
          tokens += StringLiteral(new String(stringLiteral.toArray))
          stringLiteral.clear()
        }
        isString = !isString
      case ','                                  =>
        if (isNumber) {
          isNumber = !isNumber
          tokens += NumberLiteral(new String(numberLiteral.toArray))
          numberLiteral.clear()
        }
        tokens += Comma
      case x if x >= 48 && x <= 57 && !isString =>
        if (!isNumber) isNumber = true
        numberLiteral += x
      case x                                    =>
        if (isString) stringLiteral += x
        if (isNumber) numberLiteral += x
      case _                                    =>
    }
    tokens
  }
}
