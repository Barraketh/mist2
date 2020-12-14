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
        "&&" -> `&&`,
        "||" -> `||`,
        "(" -> `(`,
        ")" -> `)`,
        "{" -> `{`,
        "}" -> `}`,
        "[" -> `[`,
        "]" -> `]`,
        "\n" -> `\n`,
        "," -> `,`,
        "=" -> `=`,
        "." -> `.`,
        "+" -> `+`,
        "-" -> `-`,
        "*" -> `*`,
        "/" -> `/`
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

    (whitespace.rep() ~ token).rep() ~ whitespace.rep() ~ End
  }

  def lex(s: String): ArrayBuffer[Token] = {
    lexGrammar.parse(s) match {
      case PSuccess(res) => res.value
      case p: PFail => throw new RuntimeException(p.toString)
    }
  }

}

case class Token(value: TokenV, start: Int, end: Int)

sealed trait TokenV
object TokenV {

  // Single char tokens
  case object `{` extends TokenV
  case object `}` extends TokenV
  case object `(` extends TokenV
  case object `)` extends TokenV
  case object `[` extends TokenV
  case object `]` extends TokenV
  case object `\n` extends TokenV
  case object `,` extends TokenV
  case object `=` extends TokenV
  case object `.` extends TokenV
  case object `+` extends TokenV
  case object `-` extends TokenV
  case object `*` extends TokenV
  case object `/` extends TokenV

  // Two char tokens (have to be handled first)
  // Eq would normally be called `==`, but scala already has that predefined in a way that conflicts
  case object EqEq extends TokenV
  case object `&&` extends TokenV
  case object `||` extends TokenV

  // Multi-char tokens
  case class StringToken(value: String) extends TokenV
  case class Ident(value: String) extends TokenV
  case class Number(intPart: String, expPart: Option[String] = None, positive: Boolean = true) extends TokenV
}
