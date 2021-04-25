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
import StringParsers._

import org.scalatest.EitherValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringParsersSpec extends AnyFlatSpec with Matchers {
  "string parser" should "parse string prefix" in {
    val string: Parser[String] = StringParsers.string("scala")
    StringParsers.run(string)("scala and java").value should be("scala")
  }

  "listOfN combinator" should "parse string prefix and return list of results" in {
    val strings: Parser[List[String]] = listOfN(3, StringParsers.string("A"))
    StringParsers.run(strings)("AAABBBCCC").value should be(List("A", "A", "A"))
  }
}
