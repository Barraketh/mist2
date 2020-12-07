package com.mistlang.parser

class Lexer {

//  val lexGrammer: ValueParser[Token] = {
//
//    val simpleTokenMap = Map[String, TokenV](
//      "==" -> EqEq,
//      "&&" -> And,
//      "||" -> Or,
//      "(" -> OpenParen,
//      ")" -> CloseParen,
//      "[" -> OpenBrace,
//      "]" -> CloseBrace,
//      "\n" -> Newline,
//      "," -> Comma,
//      "=" -> Eq,
//      "." -> Period,
//      "+" -> Plus,
//      "-" -> Minus,
//      "*" -> Mult,
//      "/" -> Div
//    )
//
//    val p = simpleTokenMap.map(e => Exact(e._1).!)
//
//  }

}

case class Token(value: TokenV, start: Int, end: Int)


sealed trait TokenV
object TokenV {


  // Single char tokens
  case object OpenBrace extends TokenV
  case object CloseBrace extends TokenV
  case object OpenParen extends TokenV
  case object CloseParen extends TokenV
  case object Newline extends TokenV
  case object Comma extends TokenV
  case object Eq extends TokenV
  case object Period extends TokenV
  case object Plus extends TokenV
  case object Minus extends TokenV
  case object Mult extends TokenV
  case object Div extends TokenV

  // Two char tokens (have to be handles first)
  case object EqEq extends TokenV
  case object And extends TokenV
  case object Or extends TokenV

  // Multi-char tokens
  case class StringT(value: String) extends TokenV
  case class Ident(value: String) extends TokenV
  case class Number(value: String) extends TokenV
}
