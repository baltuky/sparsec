/*
 * Copyright © 2021, Yauheni Baltukha <yauheni.baltukha@gmail.com>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 */

package scala.sparsec

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.matching.Regex

object StringParsers extends Parsers[Parser] {
  override def string(s: String): Parser[String] =
    (state: ParsingState) => {
      val i: Int = indexOfNonMatching(state.slice(s.length), s, 0)
      if (i == -1) {
        Success(s, s.length)
      } else {
        val found: String =
          if (state.input.length == state.offset) "end of source" else s"'${state.slice(s.length)}'"

        Failure(
          state.advanceBy(i).toError(s"string '$s' expected but $found found"),
          isCommitted = i != 0
        )
      }
    }

  @tailrec
  private def indexOfNonMatching(source: String, pattern: String, offset: Int): Int = {
    if (offset < source.length && offset < pattern.length && source(offset) == pattern(offset))
      indexOfNonMatching(source, pattern, offset + 1)
    else if (pattern.length <= offset) -1
    else offset
  }

  override def regex(r: Regex): Parser[String] =
    (state: ParsingState) => {
      val source: String = state.input.substring(state.offset)
      r.findPrefixOf(source) match {
        case Some(prefix) => Success(prefix, prefix.length)
        case None =>
          val found: String = source.headOption.map(_.toString).getOrElse("end of source")
          Failure(
            state.toError(s"string matching regex '$r' expected but $found found"),
            isCommitted = false
          )
      }
    }

  override def many[A](p: Parser[A]): Parser[List[A]] = (state: ParsingState) => {
    @tailrec
    def go(matches: List[A], offset: Int): ParsingResult[List[A]] =
      p(state.advanceBy(offset)) match {
        case Success(value, consumed) => go(value :: matches, offset + consumed)
        case Failure(_, _)            => Success(matches.reverse, offset)
      }

    go(Nil, offset = 0)
  }

  override def slice[A](p: Parser[A]): Parser[String] =
    (state: ParsingState) =>
      p(state) match {
        case Success(_, consumed) => Success(state.slice(consumed), consumed)
        case failure: Failure     => failure
    }

  override def commit[A](p: Parser[A]): Parser[A] =
    (state: ParsingState) =>
      p(state) match {
        case Failure(error, _)        => Failure(error, isCommitted = true)
        case result: ParsingResult[A] => result
    }

  override def attempt[A](p: Parser[A]): Parser[A] =
    (state: ParsingState) =>
      p(state) match {
        case Failure(error, _)        => Failure(error, isCommitted = false)
        case result: ParsingResult[A] => result
    }

  override def succeed[A](a: A): Parser[A] = (_: ParsingState) => Success(a, 0)

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    (state: ParsingState) =>
      p1(state) match {
        case Failure(_, false)        => p2(state)
        case result: ParsingResult[A] => result
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    (state: ParsingState) =>
      p(state) match {
        case Success(a, consumed) =>
          f(a)(state.advanceBy(consumed))
            .commitIf(consumed != 0)
            .advance(consumed)
        case failure: Failure => failure
    }

  override def scope[A](message: String)(p: Parser[A]): Parser[A] =
    (state: ParsingState) => p(state).mapError(_.push(state.position, message))

  override def label[A](message: String)(p: Parser[A]): Parser[A] =
    (state: ParsingState) => p(state).mapError(_.copy(List(state.position -> message)))

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(ParsingState(input)).extract
}
