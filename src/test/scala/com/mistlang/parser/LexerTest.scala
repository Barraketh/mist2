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

    val expected = ArrayBuffer[TokenV](`\n`, Ident("val"), Ident("s"), `=`, `{`, `\n`,
      Ident("if"), `(`, Number("4"), `-`, Number("5", positive =  false), EqEq, `=`, Number("3", Some("5")),
      `||`, Ident("true"), `)`, `{`, `\n`,
      Ident("myFunc"), `(`, Ident("foo"), `,`, Ident("bar"), `,`, StringToken("blammo"), `)`, `.`,
      Ident("baz"), `\n`, `}`, `\n`, `\n`,`}`, `\n`)

    val res: ArrayBuffer[TokenV] = lexer.lex(s).map(_.value)
    res shouldEqual expected
  }
}
