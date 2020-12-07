package com.mistlang.parser

import scala.collection.mutable.ArrayBuffer

sealed trait ParseResult {
  def isSuccess: Boolean
}

sealed trait ParseSuccess extends ParseResult {
  override def isSuccess: Boolean = true
}

sealed trait ParseFail extends ParseResult {
  override def isSuccess: Boolean = false
}

sealed trait UnitResult extends ParseResult {
  def pMap[Res <: ParseResult](f: (Int, Int) => Res): Res = this match {
    case u: Unmatched => u.asInstanceOf[Res]
    case Matched(start, end) => f(start, end)
  }
}
sealed trait ValueResult[+T] extends ParseResult {
  def pMap[Res <: ParseResult](f: (T, Int, Int) => Res): Res = this match {
    case u: Unmatched => u.asInstanceOf[Res]
    case Value(v, start, end) =>
      f(v, start, end) // TODO: how do I actually fix the variance?
  }

  def map[U](f: T => U): ValueResult[U]
}

case class Matched(start: Int, end: Int) extends UnitResult with ParseSuccess
case class Value[T](value: T, start: Int, end: Int) extends ValueResult[T] with ParseSuccess {
  override def map[U](f: T => U): ValueResult[U] = Value(f(value), start, end)
}
case class Unmatched(startIdx: Int, curIdx: Int, message: String)
  extends UnitResult
    with ValueResult[Nothing]
    with ParseFail {
  override def map[U](f: Nothing => U): ValueResult[U] = this
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

  trait Parser {
    def parse(startIdx: Int)(implicit seq: Repr): ParseResult
  }

  trait UnitParser extends Parser {
    override def parse(startIdx: Int)(implicit seq: Repr): UnitResult

    def ! : ValueParser[Repr] = Capture(this)

    def ~(other: UnitParser): UnitParser = AndParser(this, other)

    def ~[V](other: ValueParser[V]): ValueParser[V] = AndParser(this, other)

    def rep(min: Int = 0): UnitParser = Rep(this, min)

    def |(other: UnitParser): UnitParser = {
      (this, other) match {
        case (UnitOr(myParsers), UnitOr(otherParsers)) => UnitOr(myParsers ::: otherParsers)
        case (UnitOr(myParsers), o) => UnitOr(myParsers :+ o)
        case (m, UnitOr(otherParsers)) => UnitOr(m :: otherParsers)
        case (m, o) => UnitOr(m :: o :: Nil)
      }
    }
  }

  trait ValueParser[T] extends Parser { self =>
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T]

    def ~(other: UnitParser): ValueParser[T] = new ValueParser[T] {
      override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[T] = self.parse(startIdx)
    }//AndParser(this, other)

    def ~[V](other: ValueParser[V]): ValueParser[(T, V)] = AndParser(this, other)

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

    def map[V](f: T => V): ValueParser[V] = {
      new ValueParser[V] {
        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = self.parse(startIdx).map(f)
      }
    }



    //    def pMap[V](f: (T, Int, Int) => V): ValueParser[V] = {
    //      def p(idx: Int)(implicit seq: Repr): ValueResult[T] = parse(idx)
    //
    //      new ValueParser[V] {
    //        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = p(startIdx).pMap(f)
    //      }
    //    }
  }


  case class Exact(s: Repr) extends UnitParser {
    override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = {
      var i: Int = 0
      var j: Int = startIdx
      while (i < s.length) {
        if (s(i) != seq(j))
          return Unmatched(startIdx, j, s"Expected ${s(i)}, got ${seq(j)}")
        i += 1
        j += 1
      }
      Matched(startIdx, startIdx + s.length)
    }
  }

  case class Single(pred: Elem => Boolean) extends UnitParser {
    override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = {
      if (pred(seq(startIdx))) Matched(startIdx, startIdx + 1)
      else Unmatched(startIdx, startIdx, s"Predicate doesn't match ${seq(startIdx)}")
    }
  }

  case class While(pred: Elem => Boolean) extends UnitParser {
    override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = {
      var curIdx = startIdx
      while (curIdx < seq.length && pred(seq(curIdx))) {
        curIdx += 1
      }
      Matched(startIdx, curIdx)
    }
  }

  case class Capture(p: UnitParser) extends ValueParser[Repr] {
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[Repr] = {
      p.parse(startIdx) match {
        case Matched(start, end) =>
          val buffer = new ArrayBuffer[Elem](end - start)
          for (i <- start until end) {
            buffer += seq(i)
          }
          Value(elemSeq.build(buffer), start, end)
        case t: Unmatched => t
      }
    }
  }

  object AndParser {
    def and(first: Matched, second: Matched) = Matched(first.start, second.end)

    def and[T](first: Matched, second: Value[T]) = Value[T](second.value, first.start, second.end)

    def and[T](first: Value[T], second: Matched) = Value[T](first.value, first.start, second.end)

    def and[T, U](first: Value[T], second: Value[U]) = Value[(T, U)](
      (first.value, second.value), first.start, second.end)

    def apply[U, V](first: ValueParser[U], second: ValueParser[V]): ValueParser[(U, V)] =
      new ValueParser[(U, V)] {
        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[(U, V)] = {
          first.parse(startIdx).pMap {
            case (v1, start1, end1) =>
              second.parse(end1).pMap {
                case (v2, _, end2) => Value((v1, v2), start1, end2)
              }
          }
        }
      }

    def apply[V](first: ValueParser[V], second: UnitParser): ValueParser[V] =
      new ValueParser[V] {
        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = {
          first.parse(startIdx).pMap {
            case (v, start1, end1) =>
              second.parse(end1).pMap {
                case (_, end2) => Value(v, start1, end2)
              }
          }
        }
      }

    def apply[V](first: UnitParser, second: ValueParser[V]): ValueParser[V] =
      new ValueParser[V] {
        override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[V] = {
          first.parse(startIdx).pMap {
            case (start, end1) =>
              second.parse(end1).pMap {
                case (v, _, end2) => Value(v, start, end2)
              }
          }
        }
      }

    def apply(first: UnitParser, second: UnitParser): UnitParser =
      new UnitParser {
        override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = first.parse(startIdx).pMap {
          case (start, end1) =>
            second.parse(end1).pMap {
              case (_, end2) => Matched(start, end2)
            }
        }
      }
  }

  object Rep {
    def apply(p: UnitParser, min: Int): UnitParser = new UnitParser {
      override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = {
        var curIdx = startIdx
        var count = 0

        var curMatch = p.parse(curIdx)

        while (curMatch.isSuccess) {
          count += 1
          val m = curMatch.asInstanceOf[Matched]
          curIdx = m.end

          curMatch = p.parse(curIdx)
        }

        if (count >= min) Matched(startIdx, curIdx)
        else Unmatched(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
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

          if (count >= min) Value(buffer, startIdx, curIdx)
          else Unmatched(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
        }
      }
  }

  case class UnitOr(parsers: List[UnitParser]) extends UnitParser {
    override def parse(startIdx: Int)(implicit seq: Repr): UnitResult = {
      var i: Int = 0
      while (i < parsers.length) {
        val res = parsers(i).parse(startIdx)
        if (res.isSuccess) return res

        i += 1
      }
      Unmatched(startIdx, startIdx, "No parser matches")
    }
  }


  case class ValueOr[U](parsers: List[ValueParser[_ <: U]]) extends ValueParser[U] {
    override def parse(startIdx: Int)(implicit seq: Repr): ValueResult[U] = {
      var i: Int = 0
      while (i < parsers.length) {
        val res = parsers(i).parse(startIdx)
        if (res.isSuccess) return res

        i += 1
      }
      Unmatched(startIdx, startIdx, "No parser matches")
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

