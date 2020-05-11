package com.mistlang.parser

import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.ArrayBuffer

class LexerTest {

  import Token._

  @Test
  def testTokenize(): Unit = {
    val lexer = new Lexer
    val s =
      """
        |val  s =   {
        |  myFunc(foo, bar).baz
        |}
        |""".stripMargin

    val expected = ArrayBuffer[Token](Newline, StringToken("val"), StringToken("s"), Equal, OpenBrace, Newline, 
      StringToken("myFunc"), OpenParen, StringToken("foo"), Comma, StringToken("bar"), CloseParen, Period, 
      StringToken("baz"), Newline, CloseBrace, Newline)

    val res: ArrayBuffer[Token] = lexer.tokenize(s)
    assertEquals(res, expected)
  }
}
