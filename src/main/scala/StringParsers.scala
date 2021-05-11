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

package scala.parser

import Parsers.Parser

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.matching.Regex

object StringParsers extends Parsers[Parser] {
  override def string(s: String): Parser[String] =
    (state: ParsingState) => {
      if (state.input.startsWith(s, state.offset))
        Success(state.slice(s.length), s.length)
      else Failure(state.toError(s"Input string doesn't start with `$s`"))
    }

  override def regex(r: Regex): Parser[String] =
    (state: ParsingState) =>
      r.findPrefixOf(state.input.substring(state.offset)) match {
        case Some(prefix) => Success(prefix, prefix.length)
        case None =>
          Failure(state.toError(s"Input prefix doesn't match to `${r.toString()}` regex"))
    }

  override def many[A](p: Parser[A]): Parser[List[A]] = (state: ParsingState) => {
    @tailrec
    def go(matches: List[A], offset: Int): ParsingResult[List[A]] =
      p(state.advanceBy(offset)) match {
        case Success(value, consumed) => go(value :: matches, offset + consumed)
        case Failure(_)               => Success(matches.reverse, offset)
      }

    go(Nil, offset = 0)
  }

  override def slice[A](p: Parser[A]): Parser[String] =
    (state: ParsingState) =>
      p(state) match {
        case Success(_, consumed) => Success(state.slice(consumed), consumed)
        case failure: Failure     => failure
    }

  override def succeed[A](a: A): Parser[A] = (_: ParsingState) => Success(a, 0)

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    (state: ParsingState) =>
      p1(state) match {
        case Failure(_)          => p2(state)
        case success: Success[A] => success
    }

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    (state: ParsingState) =>
      p(state) match {
        case Success(a, consumed) => f(a)(state.advanceBy(consumed)).advance(consumed)
        case failure: Failure     => failure
    }

  override def scope[A](message: String)(p: Parser[A]): Parser[A] =
    (state: ParsingState) => p(state).mapError(_.push(state.position, message))

  override def label[A](message: String)(p: Parser[A]): Parser[A] =
    (state: ParsingState) => p(state).mapError(_.copy(List(state.position -> message)))

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(ParsingState(input)).extract
}
