package com.mistlang.parser

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


class Lexer {

  import Token._

  private val SEP = "\n\t {}(),.="

  private def splitWithSeparators(s: String, sep: String): ArrayBuffer[Token] = {
    val buffer = ArrayBuffer[Token]()
    var cur = s

    @inline
    def processCharToken(idx: Int): Unit = {
      val c = cur(idx)
      if (c == ' ' || c == '\t') {}
      else {
        buffer.append(tokenizeSingle(c))
      }
      cur = cur.substring(idx + 1)
    }

    while (cur.nonEmpty) {
      val next = cur.indexWhere(c => sep.contains(c))
      if (next == 0) {
        processCharToken(0)
      } else if (next > 0) {
        buffer.append(StringToken(cur.substring(0, next)))
        processCharToken(next)
      } else {
        buffer.append(StringToken(cur))
        cur = ""
      }
    }

    buffer
  }

  @inline
  private def tokenizeSingle(s: Char): Token = s match {
    case '{' => OpenBrace
    case '}' => CloseBrace
    case '(' => OpenParen
    case ')' => CloseParen
    case ',' => Comma
    case '=' => Equal
    case '.' => Period
    case '\n' => Newline
  }

  def tokenize(s: String): ArrayBuffer[Token] = {
    splitWithSeparators(s, SEP)
  }
}

enum Token {

  case StringToken(s: String)

  case OpenBrace, CloseBrace, OpenParen, CloseParen, Newline, Comma, Equal, Period, Tab, Space
}
