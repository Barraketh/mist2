package com.mistlang.parser

import com.mistlang.parser.ParserCtx._
import org.scalatest.{FunSuite, Matchers}


class ParserTest extends FunSuite with Matchers {
  test("Successfully parse a simple grammar") {
    val p = ("abc".p ~ "d".rep(3).! ~ "e").map(_.length)
    implicit val seq: String = "abcdddddeee"
    val res = p.parse(0)
    res shouldEqual Value(5, 0, 9)
  }


}
