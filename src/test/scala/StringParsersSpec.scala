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

import Parsers.instances._
import StringParsers._

import org.scalatest.EitherValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringParsersSpec extends AnyFlatSpec with Matchers {
  "string parser" should "parse string prefix" in {
    string("scala").run("scala and java").value should be("scala")
  }

  "listOfN combinator" should "parse string prefix and return list of results" in {
    listOfN(3, string("A")).run("AAABBBCCC").value should be(List("A", "A", "A"))
  }

  "or combinator" should "execute right parser when left has failed" in {
    val stringOrNumber: Parser[String] = string("Scala") | string("11")
    stringOrNumber.run("Scala").value should be("Scala")
    stringOrNumber.run("11").value should be("11")
    stringOrNumber.run("Java") should be(Symbol("Left"))
  }

  "many combinator" should "parse string and return a list of matches" in {
    val many: Parser[List[String]] = string("AB").*
    many.run("ABABABC").value should be(List("AB", "AB", "AB"))
    many.run("AAAABBBB").value should be(Nil)
    many.run("").value should be(Nil)
  }

  "many1 combinator" should "parse string and return a list of matches(at least one), otherwise - fail" in {
    val count1: Parser[List[String]] = string("Scala").+
    count1.run("") should be(Symbol("left"))
    count1.run("ScalaScalaSCALA").value should be(List("Scala", "Scala"))
  }

  "product combinator" should "allow to combine two parsers into one" in {
    val p: Parser[(List[String], List[String])] = string("Scala").* ** string("Java").+
    p.run("ScalaScalaJava").value should be((List("Scala", "Scala"), List("Java")))
    p.run("Scala") should be(Symbol("left"))
    p.run("") should be(Symbol("left"))
  }

  "slice" should "return input string substring" in {
    val slice: Parser[String] = string("Java").*.slice
    slice.run("JavaJavaJava").value should be("JavaJavaJava")
    slice.run("ScalaScala").value should have size 0
    slice.run("").value should have size 0
  }

  "regex parser" should "return input prefix matching to regular expression" in {
    val regex: Parser[String] = StringParsers.regex("[a-zA-Z0-9]+".r)
    regex.run("Java11").value should be("Java11")
    regex.run("Scala 13").value should be("Scala")
    regex.run("") should be(Symbol("left"))
    regex.run("___") should be(Symbol("left"))
  }

  "skipLeft combinator" should "skip parsed left value" in {
    val number: Parser[Int] = StringParsers.regex("[a-zA-Z]*".r) *> StringParsers
      .regex("[0-9]+".r)
      .map(_.toInt)

    number.run("Java15").value should be(15)
    number.run("13").value should be(13)
  }

  "skipRight combinator" should "skip parsed right value" in {
    val int: Parser[Int]          = StringParsers.regex("[0-9]+".r).map(_.toInt)
    val range: Parser[(Int, Int)] = (int <* string(" to ")) ** int

    range.run("10 to 15").value should be((10, 15))
  }

  "surrounded combinator" should "skip parsed left and right values" in {
    val quote: Parser[String] = string("'")
    val int: Parser[Int]      = StringParsers.regex("[0-9]+".r).map(_.toInt)

    surrounded(quote, quote)(int).run("'101'").value should be(101)
  }

  "as combinator" should "return specified value instead of parsing result" in {
    StringParsers.regex("\\w*".r).as(1337).run("java").value should be(1337)
  }

  "split combinator" should "use a separator parser to parse split values" in {
    val int: Parser[String]        = StringParsers.regex("[0-9]*".r)
    val spaces: Parser[String]     = StringParsers.regex("\\s+".r)
    val ints: Parser[List[String]] = int.split(spaces)
    ints.run("1 2 3 4 5").value should be(List("1", "2", "3", "4", "5"))
  }

  "scope combinator" should "allow to add messages into parser error" in {
    val error: ParseError = ParseError(
      List(
        OffsetPosition("Java", 0) -> "parsing a string",
        OffsetPosition("Java", 0) -> "string 'Scala' expected but 'Java' found",
      )
    )
    string("Scala").scope("parsing a string").run("Java") should be(Left(error))
  }

  "label combinator" should "replace parser error message with a provided one" in {
    string("Java").label("Wrong input").run("Scala") should be(
      Left(
        ParseError(
          List(
            OffsetPosition("Scala", 0) -> "Wrong input"
          ))
      ))
  }

  "optional combinator" should "make a parser optional for application" in {
    val p: Parser[((String, Option[Int]), String)] = string("Java") ** StringParsers
      .regex("[0-9]+".r)
      .?
      .map(_.map(_.toInt)) ** string("!")

    p.run("Java7!").value should be((("Java", Some(7)), "!"))
    p.run("Java!").value should be((("Java", None), "!"))
    p.run("11!") should be(Symbol("left"))
  }

  "char parser" should "match a single character" in {
    char('>').run("> echo").value should be('>')
    char('!').run("!").value should be('!')
    char('?').run("") should be(Symbol("left"))
  }

  "int parser" should "parse an integer value" in {
    int.run("+17").value should be(17)
    int.run("512").value should be(512)
    int.run("0").value should be(0)
    int.run("+0").value should be(0)
    int.run("-0").value should be(0)
    int.run("-42").value should be(-42)
    int.run("-") should be(Symbol("left"))
    int.run("+") should be(Symbol("left"))
    int.run("") should be(Symbol("left"))
  }

  "eoi parser" should "guarantee input end is met" in {
    (string("Scala") <* eoi).run("Scala").value should be("Scala")
    eoi.run("") should be(Symbol("right"))

    (char('!') <* eoi).run("! ") should be(Symbol("left"))
    eoi.run(".") should be(Symbol("left"))
  }

  "whitespace parser" should "recognize whitespace characters" in {
    whitespace.run("\n").value should be('\n')
    whitespace.run("\t").value should be('\t')
    whitespace.run("\r").value should be('\r')
    whitespace.run(" ").value should be(' ')
    whitespace.*.run("\n\r \t \n\n").value should be(List('\n', '\r', ' ', '\t', ' ', '\n', '\n'))
  }

  "token parser" should "parse sequence of values separated by whitespaces" in {
    val tokens: Parser[List[String]] = token(StringParsers.regex("[a-zA-Z0-9]+".r)).*
    tokens.run("Java").value should be(List("Java"))
    tokens.run("Java and Scala").value should be(List("Java", "and", "Scala"))
    tokens.run("Scala   13   ").value should be(List("Scala", "13"))
    tokens.run("").value should be(Nil)
  }

  "thru parser" should "consume input string until specified string met (inclusively)" in {
    thru(",").run("To be, or not to be").value should be("To be,")
    thru(".").run("No man is an island.").value should be("No man is an island.")
    thru(".").run(".").value should be(".")
    thru("!").run("") should be(Symbol("left"))
  }

  "quoted parser" should "parse input enclosed into double quotes" in {
    quoted.run("\"Just a quoted string.\"").value should be("Just a quoted string.")
    quoted.run("\"\"").value shouldBe empty
    quoted.run("\"") should be(Symbol("left"))
  }

  "double parser" should "parse double values" in {
    double.run("+1").value should be(1.0)
    double.run("-1").value should be(-1.0)
    double.run("3.14").value should be(3.14)
    double.run("0.7").value should be(0.7)
    double.run("0").value should be(0.0)
    double.run("1e2").value should be(100.0)
    double.run("1e+3").value should be(1000.0)
    double.run("10e-1").value should be(1.0)
    double.run("1E+1").value should be(10.0)
    double.run("1E-2").value should be(0.01)
    double.run("-") should be(Symbol("left"))
    double.run("") should be(Symbol("left"))
  }

  "commit combinator" should "prevent backtracking and propagate an error" in {
    val parser: Parser[String] = char('$') *> {
      char('>') *> string("UNKNOWN").commit.label("Failed to parse UNKNOWN value") |
        char('>') *> string("BACKTRACKED") |
        char('1') ** whitespace.* ** char('-') ** whitespace.* *> string("RED") |
        char('2') ** whitespace.* ** char('-') ** whitespace.* *> string("GREEN") |
        char('3') ** whitespace.* ** char('-') ** whitespace.* *> string("BLUE")
    }

    parser.run("$1 - RED").value should be("RED")
    parser.run("$2 - GREEN").value should be("GREEN")
    parser.run("$3 - BLUE").value should be("BLUE")
    parser.run("$>UNKNOWN").value should be("UNKNOWN")
    parser.run("$>BACKTRACKED") should be(Symbol("left"))
  }

  "attempt combinator" should "uncommit a parser and allow backtracking" in {
    (string("III").commit.attempt | string("IIX")).run("IIX").value should be("IIX")
  }
}
