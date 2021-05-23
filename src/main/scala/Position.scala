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

sealed trait Position {
  def line: Int
  def column: Int
  def contents: String
  def caret: String              = contents.take(column - 1).map(ch => if (ch == '\t') ch else '-') + "^"
  def <(that: Position): Boolean = line < that.line || line == that.line && column < that.column
}

final case class OffsetPosition(source: String, offset: Int) extends Position {
  override def line: Int = source.slice(0, offset + 1).count(_ == '\n') + 1

  override def column: Int = source.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1    => offset + 1
    case start => offset - start
  }

  override def contents: String =
    if (1 < source.length) source.linesIterator.drop(line - 1).next else ""

  override def toString: String = s"$line.$column"
}
