package com.mistlang.parser

import scala.collection.mutable.ArrayBuffer

/**
 * A result of a successful parse.
 */
sealed trait PValue {
  // The start of the matched input
  def start: Int

  // The end of the matched input
  def end: Int
}

/**
 * A parse value that only consumes input
 */
case class Matched(start: Int, end: Int) extends PValue

/**
 * A parse value that both consumes input and carries a value
 */
case class Value[+T](value: T, start: Int, end: Int) extends PValue

/**
 * Result of a parse
 */
sealed trait PResult[+Res <: PValue] {
  def isSuccess: Boolean

  def get: Res

  def map[U <: PValue](f: Res => U): PResult[U]

  def flatMap[U <: PValue](f: Res => PResult[U]): PResult[U]
}

case class PSuccess[Res <: PValue](res: Res) extends PResult[Res] {
  override def isSuccess: Boolean = true

  override def get: Res = res

  override def map[U <: PValue](f: Res => U): PResult[U] = PSuccess(f(res))

  override def flatMap[U <: PValue](f: Res => PResult[U]): PResult[U] = f(res)
}

case class PFail(startIdx: Int, curIdx: Int, message: String) extends PResult[Nothing] {
  override def isSuccess: Boolean = false

  override def get: Nothing = throw new RuntimeException("Calling 'get' on a failed parse")

  override def map[U <: PValue](f: Nothing => U): PResult[U] = this

  override def flatMap[U <: PValue](f: Nothing => PResult[U]): PResult[U] = this
}

/**
 * Operations on sequence of type Repr
 * @tparam Elem : Underlying element of the sequence
 * @tparam Repr : Type of the sequence
 */
trait ElemSeq[Elem, Repr] {
  def length(r: Repr): Int

  def apply(r: Repr, idx: Int): Elem

  def slice(r: Repr, from: Int, until: Int): Repr
}

/**
 * Convenient way to carry shared context amongst all the parsers in a given run.
 * Currently only used to share Elem and Repr, but probably seq should also be here instead of as an implicit
 * on all the Parser.parse() methods.
 *
 * The sequence type and operations are given by Elem, Repr, ElemSeq.  We do this because we want to be able to
 * operate on both Arrays and Strings, so we need to introduce an abstraction. Currently, we only need to index
 * and take subsequences.
 *
 * @tparam Elem : single element of the sequence
 * @tparam Repr : the sequence representation.
 * @param elemSeq : the necessary operations on Repr
 */
class ParserContext[Elem, Repr](implicit elemSeq: ElemSeq[Elem, Repr]) {
  // Matching Parser. Consumes input but carries no value
  type MParser = Parser[Matched]

  // Value Parser. Consumes input and carries a value
  type VParser[T] = Parser[Value[T]]

  type MRes = PResult[Matched]
  type VRes[T] = PResult[Value[T]]

  /**
   * The base of the parser combinator library
   */
  trait Parser[Val <: PValue] {

    /**
     * The key function of a parser. Tries to match the input starting at startIdx.
     * If successful, will return a PSuccess[Val], which carries the parsed value,
     * the start (startIdx) and the end. The next parser will start at the end index,
     * thus consuming the input
     *
     * PFAil indicates a parse failure and does not consume any input.
     *
     * @param startIdx : Index in the seq where to start parsing
     * @param seq : The sequence to parse.
     *            // TODO: Should this be part of the ParseContext?
     * @return PResult
     */
    def parse(startIdx: Int)(implicit seq: Repr): PResult[Val]

    /**
     * Parse and then check if the result has consumed the entire sequence.
     */
    def run(startIdx: Int)(implicit seq: Repr): PResult[Val] = {
      parse(startIdx) match {
        case p@PSuccess(res) if res.end == seq.length => p
        case p: PSuccess[Val] =>
          PFail(startIdx,
            p.res.end,
            s"Failed to consume input starting at ${p.res.end}")
        case p: PFail => p
      }
    }

    /**
     * Creates parser that runs this, and then other, and captures the results of both if both are successful
     * See AndParser for more information
     *
     * @param and : an implicit AndParser builder. And.Out lets us encode the algebra of combining values
     */
    def ~[U <: PValue](other: Parser[U])(implicit and: And[Val, U]): and.Out = and.and(this, other)

    /**
     * Creates a parser that runs this, and if unsuccessful then will try other, and will return success if
     * either parse succeeds.
     * See OrParser for more information
     *
     * @tparam U : the supertype of Val and V. This might be simpler if we could make Parser covariant,
     *           but that seems to break in a number of ways.
     * @param or : an implicit OrParser builder
     */
    def |[U >: Val <: PValue, V <: U](other: Parser[V])(implicit or: Or[U]): Parser[U] =
      or.or(this, other)

    /**
     * Creates a parser that runs this repeatedly until failure or all input is consumed.
     * Will only fail if the number of repetitions < min
     * VParser accumulates it's values in an ArrayBuffer.
     * MParser simply consumes its input
     *
     * See RepParser for more information
     */
    def rep(min: Int = 0)(implicit r: Rep[Val]): r.Out = r.rep(this, min)


    /**
     * Creates a parser that will optionally run this, but will still return a success
     * (without consuming any input) if the run fails.
     *
     * Transforms a VParser[T] into a VParser[ Option[T] ], where the Option represents
     * the success or failure of the parse.
     * MParser stays the same as always
     *
     * See OptParser for more information
     */
    def ?(implicit opt: Opt[Val]): opt.Out = opt.opt(this)

  }

  object Parser {
    implicit class MatchedOps(p: Parser[Matched]) {
      /**
       * Captures the result of the parse as a Repr.
       * This is the way we go from a MParser (which carries no value), to a VParser that does
       */
      def ! : VParser[Repr] = Capture(p)
    }

    implicit class ValueOps[T](p: VParser[T]) {
      /**
       * Standard mapping functions to map VParsers
       */
      def map[V](f: Value[T] => Value[V]): VParser[V] = new VParser[V] {
        override def parse(startIdx: Int)(implicit seq: Repr): VRes[V] =
          p.parse(startIdx).map(f)
      }

      def mapValue[V](f: T => V): VParser[V] = new VParser[V] {
        override def parse(startIdx: Int)(implicit seq: Repr): VRes[V] =
          p.parse(startIdx).map(res => Value(f(res.value), res.start, res.end))
      }
    }
  }

  /**
   * Typeclass builder for OptParser
   *
   * OptParser tries to match p, but if it fails it will still return a successful parse
   * If p is a VParser[T], we will build a VParser[ Opt[T] ], and a failure wil result in None as the value
   * If p is an MParser, we will just build an MParser
   * In both cases, on failure end = startIdx, so no input is consumed
   */
  trait Opt[T <: PValue] {
    type Out

    def opt(p1: Parser[T]): Out
  }

  object Opt {
    /**
     * Implementation of the OptParser
     *
     * @param p : Underlying parser
     * @param f : Maps the successfully parsed value. Enables the Value[T] -> Value[ Option[T] ] mapping
     * @param default : Since we return ParseSuccess on failure, need a default parsed value
     */
    class OptParser[In <: PValue, Out <: PValue](p: Parser[In], f: In => Out, default: Int => Out)
      extends Parser[Out] {

      override def parse(startIdx: Int)(implicit seq: Repr): PResult[Out] = {
        p.parse(startIdx) match {
          case PSuccess(res) => PSuccess(f(res))
          case _: PFail => PSuccess(default(startIdx))
        }
      }
    }

    /**
     * Concrete implementations & implicits for the type class
     */

    trait OptAux[V1 <: PValue, T] extends Opt[V1] {
      override type Out = T
    }

    implicit val opt1: OptAux[Matched, MParser] = new OptAux[Matched, MParser] {
      override def opt(p1: Parser[Matched]): MParser =
        new OptParser[Matched, Matched](p1, res => res, idx => Matched(idx, idx))
    }

    implicit def opt2[T]: OptAux[Value[T], VParser[Option[T]]] =
      new OptAux[Value[T], VParser[Option[T]]] {

      override def opt(p1: Parser[Value[T]]): Out = new OptParser[Value[T], Value[Option[T]]](
        p1, res => res.copy(value = Some(res.value)), idx => Value(None,idx, idx)
      )
    }
  }


  /**
   * Typeclass for building AndParser(p1, p2)
   *
   * An AndParser runs p1 and then runs p2 with the following conceptual algebra:
   *   And[ Matched, Matched ] => Matched
   *   And[ Matched, Value[T] ] => Value[T]
   *   And[ Value[T], Matched ] => Value[T]
   *   And[ Value[T], Value[U] ] => Value[T, U]
   *
   */
  trait And[V1 <: PValue, V2 <: PValue] {
    type Out

    def and(p1: Parser[V1], p2: Parser[V2]): Out
  }

  object And {

    /**
     * AndParser implementation
     * Runs p1, and then p2
     */
    trait AndParser[V1 <: PValue, V2 <: PValue, Res <: PValue] extends Parser[Res] {
      def p1: Parser[V1]
      def p2: Parser[V2]

      /**
       * Combines two successful results of p1 and p2 into single result.
       * This is what actually implements our algebra
       */
      def build(v1: V1, v2: V2): Res

      override def parse(startIdx: Int)(implicit seq: Repr): PResult[Res] = {
        for {
          res1 <- p1.parse(startIdx)
          res2 <- p2.parse(res1.end)
        } yield build(res1, res2)
      }
    }

    /**
     * Specific instances & implicits for the typeclass
     */

    class AndAux[V1 <: PValue, V2 <: PValue, T](f: (Parser[V1], Parser[V2]) => T) extends And[V1, V2] {
      override type Out = T
      override def and(p1: Parser[V1], p2: Parser[V2]): T = f(p1, p2)
    }

    implicit val and1 = new AndAux[Matched, Matched, MParser](And1)
    implicit def and2[V1] = new AndAux[Matched, Value[V1], VParser[V1]](And2(_, _))
    implicit def and3[V1] = new AndAux[Value[V1], Matched, VParser[V1]](And3(_, _))
    implicit def and4[V1, V2] = new AndAux[Value[V1], Value[V2], VParser[(V1, V2)]](And4(_, _))

    case class And1(p1: Parser[Matched], p2: Parser[Matched]) extends
      AndParser[Matched, Matched, Matched] with MParser {
      override def build(v1: Matched, v2: Matched): Matched = Matched(v1.start, v2.end)
    }

    case class And2[T](p1: Parser[Matched], p2: VParser[T])
      extends AndParser[Matched, Value[T], Value[T]] with VParser[T] {
      override def build(v1: Matched, v2: Value[T]): Value[T] = Value(v2.value, v1.start, v2.end)
    }

    case class And3[T](p1: VParser[T], p2: Parser[Matched]) extends
      AndParser[Value[T], Matched, Value[T]] with VParser[T] {
      override def build(v1: Value[T], v2: Matched): Value[T] = Value(v1.value, v1.start, v2.end)
    }

    case class And4[T, U](p1: VParser[T], p2: VParser[U]) extends
      AndParser[Value[T], Value[U], Value[(T, U)]] with VParser[(T, U)] {
      override def build(v1: Value[T], v2: Value[U]): Value[(T, U)] = Value(v1.value -> v2.value, v1.start, v2.end)
    }

  }

  /**
   * Typeclass builder for RepParser
   *
   * RepParser tries to match as many repetitions of p as it can.
   * If it succeeds, returns an accumulation of the parse results
   * If it fails to parse the min number of repetitions, returns an error
   *
   * Currently if it encounters a parser that succeeds without consuming input, the Rep parser will result with 0
   * repetitions. This is to avoid an infinite loop.
   *
   * TODO: maybe we need to distinguish between zero and non-zero consuming parsers at the type level?
   */
  trait Rep[T <: PValue] {
    type Out

    def rep(p1: Parser[T], min: Int): Out
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

    /**
     * RepParser implementation
     *
     * @tparam In : Type of input parser value
     * @tparam Out : Type of accumulated value
     * @tparam Acc : Type of accumulator
     */
    trait RepParser[In <: PValue, Out <: PValue, Acc <: Rep.Accumulator[In]] extends Parser[Out] {
      def p: Parser[In]

      def min: Int

      protected def newAccumulator: Acc

      protected def buildValue(accumulator: Acc, start: Int, end: Int): Out

      override def parse(startIdx: Int)(implicit seq: Repr): PResult[Out] = {
        var curIdx = startIdx
        var count = 0
        val buffer = newAccumulator

        var curMatch: PResult[In] = null

        while (curIdx < seq.length && {
          curMatch = p.parse(curIdx)
          curMatch.isSuccess &&
            curMatch.get.end > curMatch.get.start // Make sure that we don't have an infinite loop
        }) {
          count += 1
          curIdx = curMatch.get.end
          buffer.append(curMatch.get)
        }

        if (count >= min) PSuccess(buildValue(buffer, startIdx, curIdx))
        else PFail(startIdx, curIdx, s"Expected at least $min repetitions, found $count")
      }
    }

    case class MatchingRep(p: Parser[Matched], min: Int) extends MParser with
      RepParser[Matched, Matched, UnitAccumulator.type] {

      override protected def newAccumulator: UnitAccumulator = UnitAccumulator

      override protected def buildValue(accumulator: UnitAccumulator, start: Int, end: Int): Matched =
        Matched(start, end)
    }

    case class ValueRep[T](p: VParser[T], min: Int)
      extends VParser[ArrayBuffer[T]] with RepParser[Value[T], Value[ArrayBuffer[T]], BufferAccumulator[T]] {

      override protected def newAccumulator = new BufferAccumulator

      override protected def buildValue(accumulator: BufferAccumulator[T],
                                        start: Int,
                                        end: Int): Value[ArrayBuffer[T]] = {
        Value(accumulator.buffer, start, end)
      }
    }

    implicit val rep1 = new Rep[Matched]{
      override type Out = MParser

      override def rep(p1: Parser[Matched], min: Int): Out = MatchingRep(p1, min)
    }

    implicit def rep2[T] = new Rep[Value[T]] {
      override type Out = VParser[ArrayBuffer[T]]

      override def rep(p1: VParser[T], min: Int): Out = ValueRep(p1, min)
    }

  }


  /**
   * Or builder typeclass
   */
  trait Or[Res <: PValue] {
    def or(p1: Parser[_ <: Res], p2: Parser[_ <: Res]): Or.OrParser[Res]
  }

  object Or {

    /**
     * Given a list of parsers, tries to match them in order
     */
    case class OrParser[Res <: PValue](parsers: List[Parser[_ <: Res]]) extends Parser[Res] {
      override def parse(startIdx: Int)(implicit seq: Repr): PResult[Res] = {
        var i: Int = 0
        while (i < parsers.length) {
          val res = parsers(i).parse(startIdx)
          if (res.isSuccess) return res

          i += 1
        }
        PFail(startIdx, startIdx, "No parser matches")
      }
    }

    /**
     * Builds an OrParser, flattening the Ors as it goes, so that we don't get OrParser(a, OrParser(b, ...)))
     * Possibly unnecessary
     */
    private def build[Res <: PValue](p1: Parser[_ <: Res], p2: Parser[_ <: Res]): OrParser[Res] = {
      def getParsers(p: Parser[_ <: Res]) = p match {
        case o: OrParser[_] => o.parsers
        case p => p :: Nil
      }

      OrParser(getParsers(p1) ::: getParsers(p2))
    }

    /**
     * Implicits for Or typeclass
     */
    implicit val or1: Or[Matched] = new Or[Matched] {
      override def or(p1: Parser[_ <: Matched], p2: Parser[_ <: Matched]): OrParser[Matched] = build(p1, p2)
    }

    implicit def or2[U]: Or[Value[U]] = new Or[Value[U]] {
      override def or(p1: Parser[_ <: Value[U]], p2: Parser[_ <: Value[U]]): OrParser[Value[U]] =
        build[Value[U]](p1, p2)
    }
  }

  /**
   * Matches a sequence of Elem exactly
   */
  case class Exact(s: Repr) extends MParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MRes = {
      val endIdx = startIdx + s.length
      if (endIdx > seq.length)
        PFail(startIdx, startIdx, s"$s is longer than remaining sequence")
      else {
        val maybeMatch = seq.slice(startIdx, endIdx)
        if (maybeMatch == s) PSuccess(Matched(startIdx, endIdx))
        else
          PFail(startIdx,
            startIdx,
            s"Exact match failed: expected $s, got $maybeMatch")
      }
    }
  }

  /**
   * Matches a single element by predicate
   */
  case class Single(pred: Elem => Boolean) extends MParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MRes = {
      if (startIdx >= seq.length)
        PFail(startIdx, startIdx, s"Index $startIdx out of bounds")
      if (pred(seq(startIdx))) PSuccess(Matched(startIdx, startIdx + 1))
      else
        PFail(startIdx,
          startIdx,
          s"Predicate doesn't match ${seq(startIdx)}")
    }
  }

  /**
   * Matches while pred is true.
   * Could probably just be replaced by Single(pred).rep()
   */
  case class While(pred: Elem => Boolean) extends MParser {
    override def parse(startIdx: Int)(implicit seq: Repr): MRes = {
      var curIdx = startIdx
      while (curIdx < seq.length && pred(seq(curIdx))) {
        curIdx += 1
      }
      PSuccess(Matched(startIdx, curIdx))
    }
  }

  /**
   * Converts an MParser to a VParser that capture the result of the match in a subsequence
   * In the most basic case, Repr = String, and the result is VParser[String]
   */
  case class Capture(p: Parser[Matched]) extends VParser[Repr] {
    override def parse(startIdx: Int)(implicit seq: Repr): VRes[Repr] = {
      p.parse(startIdx) match {
        case PSuccess(Matched(start, end)) =>
          val captured = seq.slice(start, end)
          PSuccess(Value(captured, start, end))
        case t: PFail => t
      }
    }
  }

  /**
   * Some convenience methods on Repr
   */
  implicit class ReprOps(r: Repr) {
    def length: Int = elemSeq.length(r)

    def apply(idx: Int): Elem = elemSeq.apply(r, idx)

    def slice(from: Int, until: Int): Repr = elemSeq.slice(r, from, until)
  }

}

object ParserCtx {
  implicit val StrSeq: ElemSeq[Char, String] = new ElemSeq[Char, String] {
    override def length(r: String): Int = r.length

    override def apply(r: String, idx: Int): Char = r.charAt(idx)

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
