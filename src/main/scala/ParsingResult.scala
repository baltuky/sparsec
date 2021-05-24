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

final case class ParseError(stack: List[(Position, String)]) {
  def push(position: Position, message: String): ParseError = copy((position, message) :: stack)

  override def toString: String =
    if (stack.isEmpty) "[No error messages]"
    else {
      val collapsed: List[(Position, String)] = collapse(stack)
      collapsed.map(format).mkString("\n") + "\n\n" + context(collapsed.last)
    }

  private def format(error: (Position, String)): String = s"[${error._1}] failure: ${error._2}"

  private def context(last: (Position, String)): String = last._1.contents + "\n" + last._1.caret

  private def collapse(stack: List[(Position, String)]): List[(Position, String)] =
    stack
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).mkString("; "))
      .toList
      .sortBy(_._1)(Ordering.fromLessThan(_ < _))
}

sealed trait ParsingResult[+A] {
  def extract: Either[ParseError, A] =
    this match {
      case Success(a, _)     => Right(a)
      case Failure(error, _) => Left(error)
    }

  def advance(n: Int): ParsingResult[A] =
    this match {
      case Success(a, consumed) => Success(a, consumed + n)
      case _                    => this
    }

  def mapError(f: ParseError => ParseError): ParsingResult[A] = this match {
    case Failure(error, committed) => Failure(f(error), committed)
    case _                         => this
  }

  def commitIf(commit: Boolean): ParsingResult[A] = this match {
    case Failure(error, committed) => Failure(error, committed | commit)
    case _                         => this
  }
}

final case class Success[+A](get: A, consumed: Int) extends ParsingResult[A]

final case class Failure(get: ParseError, isCommitted: Boolean) extends ParsingResult[Nothing] {
  override def toString: String = get.toString
}
