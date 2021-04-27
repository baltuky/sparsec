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

  "or combinator" should "execute right parser when left has failed" in {
    val stringOrNumber: Parser[String] = string("Scala") | string("11")
    StringParsers.run(stringOrNumber)("Scala").value should be("Scala")
    StringParsers.run(stringOrNumber)("11").value should be("11")
    StringParsers.run(stringOrNumber)("Java") should be(Symbol("Left"))
  }

  "many combinator" should "parse string and return a list of matches" in {
    val many: Parser[List[String]] = string("AB").*
    StringParsers.run(many)("ABABABC").value should be(List("AB", "AB", "AB"))
    StringParsers.run(many)("AAAABBBB").value should be(Nil)
    StringParsers.run(many)("").value should be(Nil)
  }

  "many1 combinator" should "parse string and return a list of matches(at least one), otherwise - fail" in {
    val count1: Parser[List[String]] = string("Scala").+
    StringParsers.run(count1)("") should be(Symbol("left"))
    StringParsers.run(count1)("ScalaScalaSCALA").value should be(List("Scala", "Scala"))
  }

  "product combinator" should "allow to combine two parsers into one" in {
    val p: Parser[(List[String], List[String])] = string("Scala").* ** string("Java").+
    StringParsers.run(p)("ScalaScalaJava").value should be((List("Scala", "Scala"), List("Java")))
    StringParsers.run(p)("Scala") should be(Symbol("left"))
    StringParsers.run(p)("") should be(Symbol("left"))
  }

  "slice" should "return input string substring" in {
    val slice: Parser[String] = string("Java").*.slice
    StringParsers.run(slice)("JavaJavaJava").value should be("JavaJavaJava")
    StringParsers.run(slice)("ScalaScala").value should have size 0
    StringParsers.run(slice)("").value should have size 0
  }

  "regex parser" should "return input prefix matching to regular expression" in {
    val regex: Parser[String] = StringParsers.regex("[a-zA-Z0-9]+".r)
    StringParsers.run(regex)("Java11").value should be("Java11")
    StringParsers.run(regex)("Scala 13").value should be("Scala")
    StringParsers.run(regex)("") should be(Symbol("left"))
    StringParsers.run(regex)("___") should be(Symbol("left"))
  }
}
