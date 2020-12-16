package com.mistlang.peg

package com.mistlang.parser

case class Pos(start: Int, end: Int)

sealed trait PValue {
  def pos: Pos
}

case class Matched(pos: Pos) extends PValue
case class Value[+T](value: T, pos: Pos) extends PValue

type PResult[T <: PValue] = Either[PFail, T]

case class PFail(startIdx: Int, curIdx: Int, message: String)


/**
 * Operations on sequence of type Repr
 *
 * @tparam Elem : Underlying element of the sequence
 * @tparam Repr : Type of the sequence
 */
trait ElemSeq[Elem, Repr] {
  def length(r: Repr): Int

  def apply(r: Repr, idx: Int): Elem

  def slice(r: Repr, from: Int, until: Int): Repr
}

trait ParserContext[Elem, Repr](elemSeq: ElemSeq[Elem, Repr]) {

  extension (r: Repr) {
    def apply(idx: Int): Elem = elemSeq.apply(r, idx)

    def slice(start: Int, end: Int) = elemSeq.slice(r, start, end)

    def length = elemSeq.length(r)
  }

  trait Parser[A <: PValue] {
    def parse(startIdx: Int)(using seq: Repr): PResult[A]

    def ~[B <: PValue](other: Parser[B]): Parser[Sequence.Out[A, B]] = Sequence.SeqParser(this, other)
    
    def |[B <: PValue](other: Parser[B]): Parser[(A | B)] = new Or.OrParser(this, other)

    def ?(using default: Opt.Default[A] ) : Parser[Opt.Out[A]] = new Opt.OptParser(this, default)

    protected def fail(startIdx: Int, curIdx: Int) = Left(PFail(startIdx, curIdx, s"$this"))
  }

  object Parser {
    extension (p: Parser[Matched]) {
      def succeed(start: Int, end: Int): Right[Nothing, Matched] = Right(Matched(Pos(start, end)))

      def ! : Capture = Capture(p)
    }
  }

  /**
   *  Basic parsers
   **/

  // Matches exactly one Elem at a given position
  case class Exact(e: Elem) extends Parser[Matched] {
    override def parse(startIdx: Int)(using seq: Repr): PResult[Matched] = {
      if (seq(startIdx) == e) this.succeed(startIdx, startIdx)
      else fail(startIdx, startIdx)
    }
  }

  // Matches a sequence against a given position
  case class ExactSeq(s: Repr) extends Parser[Matched] {
    override def parse(startIdx: Int)(using seq: Repr): PResult[Matched] = {
      val endIdx = startIdx + s.length
      if (endIdx > seq.length) fail(startIdx, startIdx)
      else {
        val maybeMatch = seq.slice(startIdx, endIdx)
        if (maybeMatch == s) this.succeed(startIdx, endIdx)
        else fail(startIdx, startIdx)
      }
    }
  }

  // Captures a match as a Repr  
  case class Capture(p: Parser[Matched]) extends Parser[Value[Repr]] {
    override def parse(startIdx: Int)(using seq: Repr): PResult[Value[Repr]] = {
      p.parse(startIdx).map { m => 
        Value(seq.slice(m.pos.start, m.pos.end), m.pos)
      }
    }
  }

  /**
   * Combinators
   */


   
  // Represents p1 ~ p2 : will try to run p1 and if successful will then run p2
  object Sequence {
    /**
    * For the purpose of combining values, we want the following algebra:
    * 
    * Matched ~ Matched = Matched
    * Value[T] ~ Matched = Value[T]
    * Matched ~ Value[T] = Value[T]
    * Value[T] ~ Value[U] = Value[(T, U)]
    */
    type Out[A <: PValue, B <: PValue] <: PValue = A match {
      case Matched => MOut[B]
      case Value[t] => VOut[t, B]
    } 

    type MOut[B <: PValue] <: PValue = B match {
      case Matched => Matched
      case Value[t] => Value[t]
    }

    type VOut[T, B <: PValue] <: PValue = B match {
      case Matched => Value[T]
      case Value[u] => Value[(T, u)]
    }

    def out[A <: PValue, B <: PValue](a: A, b: B): Out[A, B] = {
      val pos = Pos(a.pos.start, b.pos.end)

      a match {
        case _: Matched => 
          b match {
            case _: Matched => Matched(pos)
            case v: Value[t] => Value(v.value, pos)
          }
        case v: Value[t] =>
          b match {
            case _: Matched => Value(v.value, pos)
            case w: Value[u] => Value((v.value, w.value), pos)
          }
      }
    }

    case class SeqParser[A <: PValue, B <: PValue](p1: Parser[A], p2: Parser[B]) extends Parser[Out[A, B]] {
      override def parse(startIdx: Int)(using seq: Repr): PResult[Out[A, B]] = {
        for {
          v1 <- p1.parse(startIdx)
          v2 <- p2.parse(v1.pos.end)
        } yield out(v1, v2)
      }
    }
  }

  // Represents p1 | p2 -> will run p1 and if not successful will run p2
  object Or {    
    case class OrParser[Out <: PValue](p1: Parser[_ <: Out], p2: Parser[_ <: Out]) extends Parser[Out] {
      override def parse(startIdx: Int)(using seq: Repr): PResult[Out] = {
        p1.parse(startIdx).orElse(p2.parse(startIdx))
      }
    }
  }

  // Represents p1.? -> will run p1.  If p1 fails will succeed without consuming any input
  object Opt {      
    type Out[A <: PValue] <: PValue = A match {
      case Matched => Matched
      case Value[t] => Value[Option[t]]
    }

    def opt[A <: PValue](a: A): Out[A] = a match {
      case m: Matched => m
      case v: Value[t] => Value(Some(v.value), v.pos)
    }

    trait Default[A <: PValue] {
      def default(pos: Pos): Out[A]
    }

    object Default {
      given Default[Matched] = pos => Matched(pos)
      given [T] as Default[Value[T]] = pos => Value(None, pos)
    }

    case class OptParser[A <: PValue](p: Parser[A], d: Default[A]) extends Parser[Out[A]] {
      override def parse(startIdx: Int)(using seq: Repr): PResult[Out[A]] = {
        p.parse(startIdx)
          .map(a => opt(a))
          .orElse(Right(d.default(Pos(startIdx, startIdx))))
      }
    }

  }
}


object ParserCtx {
  val StrSeq: ElemSeq[Char, String] = new ElemSeq[Char, String] {
    override def length(r: String): Int = r.length

    override def apply(r: String, idx: Int): Char = r.charAt(idx)

    override def slice(r: String, from: Int, until: Int): String =
      r.slice(from, until)
  }

  object StringCtx extends ParserContext[Char, String](StrSeq)

  implicit def strToParser(s: String): StringCtx.ExactSeq = StringCtx.ExactSeq(s)
  extension (s: String) {
    def p: StringCtx.ExactSeq = StringCtx.ExactSeq(s)
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    import ParserCtx._
    import StringCtx._

    val p = ("a".p | "b".? ~ "c").!
    
    given String = "d"
    println(p.parse(0))

  }
}





