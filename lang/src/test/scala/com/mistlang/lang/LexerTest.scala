package com.mistlang.peg

import org.junit.Test
import org.junit.Assert._
import TokenV._

import scala.collection.mutable.ArrayBuffer

class LexerTest {

  @Test
  def test(): Unit = {

    val lexer = new Lexer

    val s =
      """
        |val  s =   { // Foo
        |if (4--5=== 3.5 || true) {// Bar
        |  myFunc(foo, bar, "blammo").baz
        |}
        |
        |}
        |""".stripMargin

    val expected = ArrayBuffer[TokenV](`\n`, Ident("val"), Ident("s"), `=`, `{`, `\n`,
      Ident("if"), `(`, Number("4"), minus, Number("5", positive =  false), EqEq, `=`, Number("3", Some("5")),
      `||`, Ident("true"), `)`, `{`, `\n`,
      Ident("myFunc"), `(`, Ident("foo"), `,`, Ident("bar"), `,`, StringToken("blammo"), `)`, `.`,
      Ident("baz"), `\n`, `}`, `\n`, `\n`,`}`, `\n`)

    val res: ArrayBuffer[TokenV] = lexer.lex(s).map(_.value)
    assert(res == expected)
  }
}
