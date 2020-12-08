package com.mistlang.parser

import ParserCtx._
import StringCtx._

import scala.collection.mutable.ArrayBuffer

class Lexer {

  import TokenV._

  val lexGrammar: VParser[ArrayBuffer[Token]] = {

    val simpleToken = {
      val simpleTokenMap = List[(String, TokenV)](
        "==" -> EqEq,
        "&&" -> And,
        "||" -> Or,
        "(" -> OpenParen,
        ")" -> CloseParen,
        "{" -> OpenBrace,
        "}" -> CloseBrace,
        "[" -> OpenBracket,
        "]" -> CloseBracket,
        "\n" -> Newline,
        "," -> Comma,
        "=" -> Eq,
        "." -> Period,
        "+" -> Plus,
        "-" -> Minus,
        "*" -> Mult,
        "/" -> Div
      )

      simpleTokenMap
        .map(e => Exact(e._1).!.mapValue(_ => e._2))
        .reduceLeft((first, second) => first | second)
    }

    // TODO: Add escape sequences
    val string = ("\"" ~ While(c => c != '\n' && c != '"').! ~ "\"").mapValue(s => StringToken(s))

    val digit = CharIn('0' -> '9')

    val number = {
      val sign = CharIn("+-").!.mapValue(c => c == "+")
      (sign.? ~ digit.rep(1).! ~ ("." ~ digit.rep(1).!).?).mapValue {
        case ((sign, intPart), expPart) => Number(intPart, expPart, sign.getOrElse(true))
      }
    }

    val ident = {
      val letter = CharIn('a' -> 'z', 'A' -> 'Z')
      (letter ~ (letter | digit | "_").rep()).!.mapValue(s => Ident(s))
    }

    // Numbers go first because of sign, then simple tokens, then idents.  Strings can go anywhere in this sequence

    val token = (number | simpleToken | string | ident).map(res =>
      Value(Token(res.value, res.start, res.end), res.start, res.end))

    val whitespace = CharIn(" \t")

    (whitespace.rep() ~ token).rep() ~ whitespace.rep()
  }

  def lex(s: String): ArrayBuffer[Token] = {
    lexGrammar.run(0)(s) match {
      case PSuccess(res) => res.value
      case p: PFail => throw new RuntimeException(p.toString)
    }
  }

}

case class Token(value: TokenV, start: Int, end: Int)

sealed trait TokenV
object TokenV {

  // Single char tokens
  case object OpenBrace extends TokenV
  case object CloseBrace extends TokenV
  case object OpenParen extends TokenV
  case object CloseParen extends TokenV
  case object OpenBracket extends TokenV
  case object CloseBracket extends TokenV
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
  case class StringToken(value: String) extends TokenV
  case class Ident(value: String) extends TokenV
  case class Number(intPart: String, expPart: Option[String], positive: Boolean = true) extends TokenV
}
