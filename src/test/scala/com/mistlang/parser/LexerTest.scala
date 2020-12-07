package com.mistlang.parser

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ArrayBuffer

class LexerTest extends FunSuite with Matchers {

  import TokenV._


  test("Successfully lex a simple function ") {
    val lexer = new Lexer
    val s =
      """
        |val  s =   {
        |if (4--5=== 3.5 || true) {
        |  myFunc(foo, bar, "blammo").baz
        |}
        |
        |}
        |""".stripMargin

    val expected = ArrayBuffer[TokenV](Newline, Ident("val"), Ident("s"), Eq, OpenBrace, Newline,
      Ident("if"), OpenParen, Number("4", None), Minus, Number("5", None, false), EqEq, Eq, Number("3", Some("5")),
      Or, Ident("true"), CloseParen, OpenBrace, Newline,
      Ident("myFunc"), OpenParen, Ident("foo"), Comma, Ident("bar"), Comma, StringToken("blammo"), CloseParen, Period,
      Ident("baz"), Newline, CloseBrace, Newline, Newline,CloseBrace, Newline)

    val res: ArrayBuffer[TokenV] = lexer.lex(s).map(_.value)
    res shouldEqual expected
  }
}
