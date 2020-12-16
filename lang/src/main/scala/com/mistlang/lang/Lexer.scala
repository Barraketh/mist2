package com.mistlang.peg

import ParserCtx._
import StringCtx._

import scala.collection.mutable.ArrayBuffer

class Lexer {

  import TokenV._

  val lexGrammar: Parser[Value[ArrayBuffer[Token]]] = {

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
        "+" -> plus,
        "-" -> minus,
        "*" -> `*`,
        "/" -> `/`
      )

      simpleTokenMap
        .map(e => Exact(e._1).!.map(_ => e._2))
        .reduceLeft((first, second) => first | second)
    }

    // TODO: Add escape sequences
    val string = ("\"" ~ Single(c => c != '\n' && c != '"').rep().! ~ "\"").map(s => StringToken(s))

    val digit = CharIn('0' -> '9')

    val number = {
      val sign = CharIn("+-").!.map(c => c == "+")
      (sign.? ~ digit.rep(1).! ~ ("." ~ digit.rep(1).!).?).map {
        case ((sign, intPart), expPart) => Number(intPart, expPart, sign.getOrElse(true))
      }
    }

    val ident = {
      val letter = CharIn('a' -> 'z', 'A' -> 'Z')
      (letter ~ (letter | digit | "_").rep()).!.map(s => Ident(s))
    }

    // Numbers go first because of sign, then simple tokens, then idents.  Strings can go anywhere in this sequence

    val token = (number | simpleToken | string | ident)
      .mapValue(res => Value(Token(res.value, res.pos), res.pos))

    val whitespace = CharIn(" \t")

    (whitespace.rep() ~ token).rep() ~ whitespace.rep() ~ End
  }

  def lex(s: String): ArrayBuffer[Token] = {
    lexGrammar.parse(s) match {
      case Right(res) => res.value
      case Left(p) => throw new RuntimeException(p.toString)
    }
  }

}

case class Token(value: TokenV, pos: Pos)

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
  case object plus extends TokenV
  case object minus extends TokenV
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
