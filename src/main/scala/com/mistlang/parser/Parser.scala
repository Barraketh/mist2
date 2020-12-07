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

  override def flatMap[U <: ParsedValue](f: Res => ParseResult[U]): ParseResult[U] = f(res)
}

case class ParseFail(startIdx: Int, curIdx: Int, message: String) extends ParseResult[Nothing] {
  override def isSuccess: Boolean = false

  override def map[U <: ParsedValue](f: Nothing => U): ParseResult[U] = this

  override def flatMap[U <: ParsedValue](f: Nothing => ParseResult[U]): ParseResult[U] = this
}

sealed trait ParsedValue {
  def start: Int

  def end: Int
}

trait ParsedValueTypeOps {
  type AndMatched <: ParsedValue
  type AndValue[T] <: ParsedValue
  type Rep <: ParsedValue
  type Opt <: ParsedValue
}

trait MatchedTypeOps extends ParsedValueTypeOps {
  type AndMatched = Matched
  type AndValue[T] = Value[T]
  type Rep = Matched
  type Opt = Matched
}

trait ValueTypeOps[T] extends ParsedValueTypeOps {
  override type AndMatched = Value[T]
  override type AndValue[U] = Value[(T, U)]
  override type Rep = Value[ArrayBuffer[T]]
  override type Opt = Value[Option[T]]
}

case class Matched(start: Int, end: Int) extends ParsedValue

case class Value[+T](value: T, start: Int, end: Int) extends ParsedValue

trait ElemSeq[Elem, Repr] {
  def length(r: Repr): Int

  def apply(r: Repr, idx: Int): Elem

  def build(buffer: ArrayBuffer[Elem]): Repr

  def slice(r: Repr, from: Int, until: Int): Repr
}

class ParserContext[Elem, Repr](implicit elemSeq: ElemSeq[Elem, Repr]) {
  implicit class ReprOps(r: Repr) {
    def length: Int = elemSeq.length(r)

    def apply(idx: Int): Elem = elemSeq.apply(r, idx)

    def slice(from: Int, until: Int): Repr = elemSeq.slice(r, from, until)
  }

  trait Parser[+Res <: ParsedValue] {
    type TypeOps <: ParsedValueTypeOps

    def parse(startIdx: Int)(implicit seq: Repr): ParseResult[Res]

    def ~(other: MatchingParser): Parser[TypeOps#AndMatched]

    def ~[T](other: ValueParser[T]): Parser[TypeOps#AndValue[T]]

    def rep(min: Int = 0): Parser[TypeOps#Rep]

    def ? : Parser[TypeOps#Opt]

    def run(startIdx: Int)(implicit seq: Repr): ParseResult[Res] = {
      parse(startIdx) match {
        case p@ParseSuccess(res) if res.end == seq.length => p
        case p: ParseSuccess[Res] =>
          ParseFail(startIdx,
            p.res.end,
            s"Failed to consume input starting at ${p.res.end}")
        case p: ParseFail => p
      }
    }

  }

  trait MatchingParser extends Parser[Matched] {
    self =>

    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult

    override type TypeOps = MatchedTypeOps

    def ! : ValueParser[Repr] = Capture(this)

    def ? : MatchingParser = new MatchingParser {
      override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult =
        self.parse(startIdx) match {
          case p: ParseSuccess[Matched] => p
          case _: ParseFail => ParseSuccess(Matched(startIdx, startIdx))
        }
    }

    def ~(other: MatchingParser): MatchingParser = new MatchingParser {
      override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult =
        for {
          res1 <- self.parse(startIdx)
          res2 <- other.parse(res1.end)
        } yield Matched(res1.start, res2.end)
    }

    def ~[V](other: ValueParser[V]): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] =
        for {
          res1 <- self.parse(startIdx)
          res2 <- other.parse(res1.end)
        } yield Value(res2.value, res1.start, res2.end)
    }

    def rep(min: Int = 0): MatchingParser = Rep(this, min)

    def |(other: MatchingParser): MatchingParser = Or(this, other)
  }

  trait ValueParser[T] extends Parser[Value[T]] {
    self =>

    override type TypeOps = ValueTypeOps[T]

    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T]

    def ? : ValueParser[Option[T]] = new ValueParser[Option[T]] {
      override def parse(startIdx: Int)(
        implicit seq: Repr): ValueResult[Option[T]] =
        self.parse(startIdx) match {
          case ParseSuccess(Value(value, start, end)) =>
            ParseSuccess(Value(Some(value), start, end))
          case _: ParseFail => ParseSuccess(Value(None, startIdx, startIdx))
        }
    }

    def ~(other: MatchingParser): ValueParser[T] = new ValueParser[T] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T] =
        self.parse(startIdx).flatMap { res1 =>
          other.parse(res1.end).map { res2 =>
            Value(res1.value, res1.start, res2.end)
          }
        }
    }

    def ~[V](other: ValueParser[V]): ValueParser[(T, V)] =
      new ValueParser[(T, V)] {
        override def parse(startIdx: Int)(
          implicit seq: Repr): ValueResult[(T, V)] =
          self.parse(startIdx).flatMap { res1 =>
            other.parse(res1.end).map { res2 =>
              Value((res1.value, res2.value), res1.start, res2.end)
            }
          }
      }

    def rep(min: Int = 0): ValueParser[ArrayBuffer[T]] = Rep(this, min)

    def |[U >: T, V <: U](other: ValueParser[V]): ValueParser[U] =
      Or(this, other)

    def map[V](f: Value[T] => Value[V]): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] =
        self.parse(startIdx).map(f)
    }

    def mapValue[V](f: T => V): ValueParser[V] = new ValueParser[V] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] =
        self.parse(startIdx).map(res => Value(f(res.value), res.start, res.end))
    }
  }

  case class Exact(s: Repr) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      val endIdx = startIdx + s.length
      if (endIdx > seq.length)
        ParseFail(startIdx, startIdx, s"$s is longer than remaining sequence")
      else {
        val maybeMatch = seq.slice(startIdx, endIdx)
        if (maybeMatch == s) ParseSuccess(Matched(startIdx, endIdx))
        else
          ParseFail(startIdx,
            startIdx,
            s"Exact match failed: expected $s, got $maybeMatch")
      }
    }
  }

  case class Single(pred: Elem => Boolean) extends MatchingParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MatchedResult = {
      if (startIdx >= seq.length)
        ParseFail(startIdx, startIdx, s"Index $startIdx out of bounds")
      if (pred(seq(startIdx))) ParseSuccess(Matched(startIdx, startIdx + 1))
      else
        ParseFail(startIdx,
          startIdx,
          s"Predicate doesn't match ${seq(startIdx)}")
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

  trait Rep[In <: ParsedValue, Out <: ParsedValue, Acc <: Rep.Accumulator[In]] extends Parser[Out] {
    def p: Parser[In]

    def min: Int

    protected def newAccumulator: Acc

    protected def buildValue(accumulator: Acc, start: Int, end: Int): Out

    override def parse(startIdx: Int)(implicit seq: Repr): ParseResult[Out] = {
      var curIdx = startIdx
      var count = 0
      val buffer = newAccumulator

      var curMatch: ParseResult[In] = null

      while (curIdx < seq.length && {
        curMatch = p.parse(curIdx)
        curMatch.isSuccess
      }) {
        count += 1
        val m = curMatch.asInstanceOf[ParseSuccess[In]]
        curIdx = m.res.end
        buffer.append(m.res)
      }

      if (count >= min) ParseSuccess(buildValue(buffer, startIdx, curIdx))
      else
        ParseFail(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
    }
  }

  object Rep {
    trait Accumulator[In] {
      def append(r: In): Unit
    }
    object UnitAccumulator extends Accumulator[Matched] {
      override def append(r: Matched): Unit = ()
    }
    type UnitAccumulator = UnitAccumulator.type

    class BufferAccumulator[T] extends Accumulator[Value[T]] {
      val buffer = new ArrayBuffer[T]()

      override def append(r: Value[T]): Unit = {
        buffer += r.value
      }
    }

    case class MatchingRep(p: MatchingParser, min: Int) extends MatchingParser with
      Rep[Matched, Matched, UnitAccumulator.type] {

      override protected def newAccumulator: UnitAccumulator = UnitAccumulator

      override protected def buildValue(accumulator: UnitAccumulator, start: Int, end: Int): Matched =
        Matched(start, end)
    }

    case class ValueRep[T](p: ValueParser[T], min: Int)
      extends ValueParser[ArrayBuffer[T]] with Rep[Value[T], Value[ArrayBuffer[T]], BufferAccumulator[T]] {

      override protected def newAccumulator = new BufferAccumulator

      override protected def buildValue(accumulator: BufferAccumulator[T],
                                        start: Int,
                                        end: Int): Value[ArrayBuffer[T]] = {
        Value(accumulator.buffer, start, end)
      }
    }

    def apply(p: MatchingParser, min: Int): MatchingParser = MatchingRep(p, min)

    def apply[T](p: ValueParser[T], min: Int): ValueParser[ArrayBuffer[T]] =
      ValueRep(p, min)
  }

  trait Or[Res <: ParsedValue] extends Parser[Res] {
    def parsers: List[Parser[Res]]

    override def parse(startIdx: Int)(implicit seq: Repr): ParseResult[Res] = {
      var i: Int = 0
      while (i < parsers.length) {
        val res = parsers(i).parse(startIdx)
        if (res.isSuccess) return res

        i += 1
      }
      ParseFail(startIdx, startIdx, "No parser matches")
    }
  }

  object Or {
    case class MatchingOr(parsers: List[Parser[Matched]])
      extends MatchingParser
        with Or[Matched]

    case class ValueOr[U](parsers: List[Parser[Value[_ <: U]]])
      extends ValueParser[U]
        with Or[Value[U]]

    def apply[Res <: ParsedValue, P <: Parser[Res]](
                                                     p1: Parser[Res],
                                                     p2: Parser[Res],
                                                     f: List[Parser[Res]] => P): P = {
      def getParsers(p: Parser[Res]) = p match {
        case o: Or[Res] => o.parsers
        case p => p :: Nil
      }

      f(getParsers(p1) ::: getParsers(p2))
    }

    def apply(p1: MatchingParser, p2: MatchingParser): MatchingParser =
      apply(p1, p2, MatchingOr)

    def apply[U, V1 <: U, V2 <: U](p1: ValueParser[V1],
                                   p2: ValueParser[V2]): ValueParser[U] =
      apply[Value[U], ValueParser[U]](p1, p2, p => ValueOr[U](p))
  }

}

object ParserCtx {
  implicit val StrSeq: ElemSeq[Char, String] = new ElemSeq[Char, String] {
    override def length(r: String): Int = r.length

    override def apply(r: String, idx: Int): Char = r.charAt(idx)

    override def build(buffer: ArrayBuffer[Char]): String =
      new String(buffer.toArray)

    override def slice(r: String, from: Int, until: Int): String =
      r.slice(from, until)
  }

  object StringCtx extends ParserContext[Char, String] {
    def CharIn(ranges: (Char, Char)*): Single =
      Single(c =>
        ranges.exists {
          case (first, last) => c >= first && c <= last
        })

    def CharIn(s: String): Single = Single(c => s.contains(c))
  }

  implicit def strToParser(s: String): StringCtx.Exact = StringCtx.Exact(s)
  implicit class StrOps(s: String) {
    def p: StringCtx.Exact = StringCtx.Exact(s)
  }

}
