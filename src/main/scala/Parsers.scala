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

import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[Error, Parser[+ _]] { self =>
  def string(s: String): Parser[String]

  def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f andThen succeed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    (p1 ** p2).map(f.tupled)

  def product[A, B](p1: => Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def skipLeft[B](p1: Parser[Any], p2: => Parser[B]): Parser[B] = map2(p1.slice, p2)((_, b) => b)

  def skipRight[B](p1: => Parser[B], p2: Parser[Any]): Parser[B] = map2(p1, p2.slice)((a, _) => a)

  def run[A](p: Parser[A])(input: String): Either[String, A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B]                            = self.map(p)(f)
    def ^^[B](f: A => B): Parser[B]                             = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B]                = self.flatMap(p)(f)
    def >>[B](f: A => Parser[B]): Parser[B]                     = self.flatMap(p)(f)
    def map2[B, C](p1: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p1)(f)
    def |[B >: A](p1: => Parser[B]): Parser[B]                  = self.or(p, p1)
    def **[B](p1: => Parser[B]): Parser[(A, B)]                 = self.product(p, p1)
    def `*` : Parser[List[A]]                                   = self.many(p)
    def `+` : Parser[List[A]]                                   = self.many1(p)
    def slice: Parser[String]                                   = self.slice(p)
    def *>[B](p1: Parser[B]): Parser[B]                         = self.skipLeft(p, p1)
    def <*(p1: Parser[Any]): Parser[A]                          = self.skipRight(p, p1)
  }
}

object Parsers {
  type Parser[+A] = ParsingState => ParsingResult[A]
}
