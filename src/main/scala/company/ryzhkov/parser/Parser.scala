package company.ryzhkov.parser

import company.ryzhkov.{Json, Tree}

object Parser {
  def parse(str: String): Json = {
    import Tree._
    import Json._

    val analyzer = new Analyzer
    val result   = analyzer.analyze(Tokenizer.createTokens(str))

    def leafToJson(tree: Tree): Json = {
      tree match {
        case leaf: ListLeaf     => JsonArray(leaf.elements.map(leafToJson).toList)
        case leaf: ObjectLeaf   =>
          val pairList = leaf.elements.toList
            .grouped(2)
            .map {
              case List(a, b) =>
                val stringLeaf = a.asInstanceOf[StringLeaf]
                (stringLeaf.value, leafToJson(b))
            }
            .toList

          JsonObject(pairList)
        case NumberLeaf(value)  => JsonNumber(value)
        case StringLeaf(value)  => JsonString(value)
        case BooleanLeaf(value) => ???
        case NullLeaf           => ???
      }
    }

    leafToJson(result)
  }
}
