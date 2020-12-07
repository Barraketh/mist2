package com.mistlang.parser

import com.mistlang.parser.ParseResult.{MatchedResult, ValueResult}

import scala.collection.mutable.ArrayBuffer

sealed trait ParseResult[+Res <: ParsedValue] {
  def isSuccess: Boolean

  def map[U <: ParsedValue](f: Res => U): ParseResult[U]

  def flatMap[U <: ParsedValue](f: Res => ParseResult[U]): ParseResult[U]
}

object ParseResult {
  type ValueResult[T] = ParseResult[Value[T]]
  type MatchedResult = ParseResult[Matched]
}

case class ParseSuccess[Res <: ParsedValue](res: Res) extends ParseResult[Res] {
  override def isSuccess: Boolean = true

  override def map[U <: ParsedValue](f: Res => U): ParseResult[U] = ParseSuccess(f(res))

  override def flatMap[U <: ParsedValue](f: Res => ParseResult[U]): ParseResult[U] = {
    f(res)
  }
}

case class ParseFail(startIdx: Int, curIdx: Int, message: String) extends ParseResult[Nothing] {
  override def isSuccess: Boolean = false

  override def map[U <: ParsedValue](f: Nothing => U): ParseResult[U] = this

  override def flatMap[U <: ParsedValue](f: Nothing => ParseResult[U]): ParseResult[U] = this
}

sealed trait ParsedValue {
  def start: Int
  def end: Int

  type AndMatched <: ParsedValue
  type AndValue[T] <: ParsedValue
  type Rep <: ParsedValue
}

case class Matched(start: Int, end: Int) extends ParsedValue {
  override type AndMatched = Matched
  override type AndValue[T] = Value[T]
  override type Rep = Matched

}
case class Value[T](value: T, start: Int, end: Int) extends ParsedValue {
  override type AndMatched = Value[T]
  override type AndValue[U] = Value[(T, U)]
  override type Rep = Value[ArrayBuffer[T]]
}

trait ElemSeq[Elem, Repr] {
  def length(r: Repr): Int

  def apply(r: Repr, idx: Int): Elem

  def build(buffer: ArrayBuffer[Elem]): Repr
}

class ParserContext[Elem, Repr](implicit elemSeq: ElemSeq[Elem, Repr]) {
  implicit class ReprOps(r: Repr) {
    def length: Int = elemSeq.length(r)

    def apply(idx: Int): Elem = elemSeq.apply(r, idx)
  }

  trait Parser[Res <: ParsedValue] {
    def parse(startIdx: Int)(implicit seq: Repr): ParseResult[Res]

    def ~(other: MatchingParser): Parser[Res#AndMatched]
    def ~[T](other: ValueParser[T]): Parser[Res#AndValue[T]]
    def rep(min: Int = 0): Parser[Res#Rep]

  }

  trait MatchingParser extends Parser[Matched] { self =>
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult

    def ! : ValueParser[Repr] = Capture(this)

    def ~(other: MatchingParser): MatchingParser = new MatchingParser {
      override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult =  for {
        res1 <- self.parse(startIdx)
        res2 <- other.parse(res1.end)
      } yield (Matched(res1.start, res2.end))
    }

    def ~[V](other: ValueParser[V]): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = for {
        res1 <- self.parse(startIdx)
        res2 <- other.parse(res1.end)
      } yield (Value(res2.value, res1.start, res2.end))
    }

    def rep(min: Int = 0): MatchingParser = Rep(this, min)

    def |(other: MatchingParser): MatchingParser = {
      (this, other) match {
        case (MatchingOr(myParsers), MatchingOr(otherParsers)) => MatchingOr(myParsers ::: otherParsers)
        case (MatchingOr(myParsers), o) => MatchingOr(myParsers :+ o)
        case (m, MatchingOr(otherParsers)) => MatchingOr(m :: otherParsers)
        case (m, o) => MatchingOr(m :: o :: Nil)
      }
    }
  }

  trait ValueParser[T] extends Parser[Value[T]] { self =>
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T]

    def ~(other: MatchingParser): ValueParser[T] = new ValueParser[T] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T] = self.parse(startIdx).flatMap { res1 =>
        other.parse(res1.end).map { res2 =>
          Value(res1.value, res1.start, res2.end)
        }
      }
    }

    def ~[V](other: ValueParser[V]): ValueParser[(T, V)] = new ValueParser[(T, V)] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[(T, V)] =
        self.parse(startIdx).flatMap { res1 =>
          other.parse(res1.end).map { res2 =>
            Value((res1.value, res2.value), res1.start, res2.end)
          }
        }
    }

    def rep(min: Int = 0): ValueParser[ArrayBuffer[T]] = Rep(this, min)

    def |[U >: T, V <: U](other: ValueParser[V]): ValueParser[U] = {
      type Res = List[ValueParser[U]]

      (this, other) match {
        case (ValueOr(myParsers), ValueOr(otherParsers)) =>
          ValueOr[U]((myParsers ::: otherParsers).asInstanceOf[Res])
        case (ValueOr(myParsers), o) => ValueOr[U]((myParsers :+ o).asInstanceOf[Res])
        case (m, ValueOr(otherParsers)) => ValueOr[U]((m :: otherParsers).asInstanceOf[Res])
        case (m, o) => ValueOr((m :: o :: Nil).asInstanceOf[Res])
      }
    }

    def map[V](f: Value[T] => Value[V]): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = self.parse(startIdx).map(f)
    }

    def mapValue[V](f: T => V): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] =
        self.parse(startIdx).map(res => Value(f(res.value), res.start, res.end))
    }
  }


  case class Exact(s: Repr) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      var i: Int = 0
      var j: Int = startIdx
      while (i < s.length) {
        if (s(i) != seq(j))
          return ParseFail(startIdx, j, s"Expected ${s(i)}, got ${seq(j)}")
        i += 1
        j += 1
      }
      ParseSuccess(Matched(startIdx, startIdx + s.length))
    }
  }

  case class Single(pred: Elem => Boolean) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      if (pred(seq(startIdx))) ParseSuccess(Matched(startIdx, startIdx + 1))
      else ParseFail(startIdx, startIdx, s"Predicate doesn't match ${seq(startIdx)}")
    }
  }

  case class While(pred: Elem => Boolean) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      var curIdx = startIdx
      while (curIdx < seq.length && pred(seq(curIdx))) {
        curIdx += 1
      }
      ParseSuccess(Matched(startIdx, curIdx))
    }
  }

  case class Capture(p: MatchingParser) extends ValueParser[Repr] {
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[Repr] = {
      p.parse(startIdx) match {
        case ParseSuccess(Matched(start, end)) =>
          val buffer = new ArrayBuffer[Elem](end - start)
          for (i <- start until end) {
            buffer += seq(i)
          }
          ParseSuccess(Value(elemSeq.build(buffer), start, end))
        case t: ParseFail => t
      }
    }
  }

  object Rep {
    def apply(p: MatchingParser, min: Int): MatchingParser = new MatchingParser {
      override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
        var curIdx = startIdx
        var count = 0

        var curMatch = p.parse(curIdx)

        while (curMatch.isSuccess) {
          count += 1
          val m = curMatch.asInstanceOf[ParseSuccess[Matched]]
          curIdx = m.res.end

          curMatch = p.parse(curIdx)
        }

        if (count >= min) ParseSuccess(Matched(startIdx, curIdx))
        else ParseFail(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
      }
    }

    def apply[T](p: ValueParser[T], min: Int): ValueParser[ArrayBuffer[T]] =
      new ValueParser[ArrayBuffer[T]] {
        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[ArrayBuffer[T]] = {
          var curIdx = startIdx
          var count = 0
          val buffer = new ArrayBuffer[T]()

          var curMatch = p.parse(curIdx)

          while (curMatch.isSuccess) {
            count += 1
            val m = curMatch.asInstanceOf[Value[T]]
            curIdx = m.end
            buffer += m.value

            curMatch = p.parse(curIdx)
          }

          if (count >= min) ParseSuccess(Value(buffer, startIdx, curIdx))
          else ParseFail(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
        }
      }
  }

  case class MatchingOr(parsers: List[MatchingParser]) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      var i: Int = 0
      while (i < parsers.length) {
        val res = parsers(i).parse(startIdx)
        if (res.isSuccess) return res

        i += 1
      }
      ParseFail(startIdx, startIdx, "No parser matches")
    }
  }


  case class ValueOr[U](parsers: List[ValueParser[_ <: U]]) extends ValueParser[U] {
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[U] = {
      var i: Int = 0
      while (i < parsers.length) {
        val res = parsers(i).parse(startIdx).asInstanceOf[ValueResult[U]]
        if (res.isSuccess) return res

        i += 1
      }
      ParseFail(startIdx, startIdx, "No parser matches")
    }
  }
}

object ParserCtx {
  implicit val StrSeq: ElemSeq[Char, String] = new ElemSeq[Char, String] {
    override def length(r: String): Int = r.length

    override def apply(r: String, idx: Int): Char = r.charAt(idx)

    override def build(buffer: ArrayBuffer[Char]): String = new String(buffer.toArray)
  }

  val StringCtx = new ParserContext[Char, String]()

  implicit def strToParser(s: String): StringCtx.Exact = StringCtx.Exact(s)
  implicit class StrOps(s: String) {
    def p: StringCtx.Exact = StringCtx.Exact(s)
  }
}

