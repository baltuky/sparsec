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
package json

import Parsers.instances._
import json.JSON.parser

import org.scalatest.EitherValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JSONSpec extends AnyFlatSpec with Matchers {
  it should "parse an empty json array" in {
    parser.run("[]").value should be(JArray(IndexedSeq.empty))
  }

  it should "parse a json array of numbers" in {
    parser.run("[1, 2, 3]").value should be(
      JArray(
        IndexedSeq(
          JNumber(1.0),
          JNumber(2.0),
          JNumber(3.0)
        )))
  }

  it should "parse a json array of booleans" in {
    parser.run("[false, true, true]").value should be(
      JArray(IndexedSeq(JBool(false), JBool(true), JBool(true)))
    )
  }

  it should "parse a json array of nulls" in {
    parser.run("[null, null]").value should be(JArray(IndexedSeq(JNull, JNull)))
  }

  it should "parse a json array of strings" in {
    parser.run("""["Java", ""]""").value should be(JArray(IndexedSeq(JString("Java"), JString(""))))
  }

  it should "parse a json array of different values" in {
    parser.run("""[1, null,false,"Scala"]""").value should be(
      JArray(
        IndexedSeq(
          JNumber(1.0),
          JNull,
          JBool(false),
          JString("Scala")
        )
      ))
  }

  it should "parse a json array of objects" in {
    parser
      .run("""[{"id": 1}, {"id" : 2}, {"id": 3, "address": {"street": "Baker"}}]""")
      .value should be(
      JArray(
        IndexedSeq(
          JObject(Map("id" -> JNumber(1.0))),
          JObject(Map("id" -> JNumber(2.0))),
          JObject(
            Map(
              "id"      -> JNumber(3.0),
              "address" -> JObject(Map("street" -> JString("Baker")))
            )),
        )
      )
    )
  }

  it should "parse a json array with whitespaces" in {
    parser.run("[\n]").value should be(JArray(IndexedSeq.empty))
    parser.run("[\n1,\r2,\t3,\n4,\n\n\n 5]").value should be(
      JArray(
        IndexedSeq(
          JNumber(1.0),
          JNumber(2.0),
          JNumber(3.0),
          JNumber(4.0),
          JNumber(5.0)
        )))
    parser.run("\n [\n] ").value should be(JArray(IndexedSeq.empty))
  }

  it should "parse a json object with no keys" in {
    parser.run("{}").value should be(JObject(Map.empty))
    parser.run("{ }").value should be(JObject(Map.empty))
    parser.run("{\n}").value should be(JObject(Map.empty))
    parser.run("{\r\n}").value should be(JObject(Map.empty))
    parser.run("{\n\n}").value should be(JObject(Map.empty))
    parser.run("{\t\t\t}").value should be(JObject(Map.empty))
  }

  it should "parse a json object with numerical values" in {
    parser.run("""{"value":101}""").value should be(
      JObject(
        Map(
          "value" -> JNumber(101.0)
        )))

    parser.run("""{"value": 102}""").value should be(
      JObject(
        Map(
          "value" -> JNumber(102.0)
        )))

    parser.run("""{"value1": 102,  "value2": 103,"value3" : 104}""").value should be(
      JObject(
        Map(
          "value1" -> JNumber(102.0),
          "value2" -> JNumber(103.0),
          "value3" -> JNumber(104.0)
        )))
  }

  it should "parse a json object with string values" in {
    parser.run("""{"name": "Alex"}""").value should be(
      JObject(
        Map(
          "name" -> JString("Alex")
        )))

    parser.run("""{"name": "Mary", "age": "23"}""").value should be(
      JObject(
        Map(
          "name" -> JString("Mary"),
          "age"  -> JString("23")
        )))
  }

  it should "parse a json object with boolean values" in {
    parser.run("""{"isError": false}""").value should be(
      JObject(
        Map(
          "isError" -> JBool(false)
        )))

    parser.run("""{"isError": true}""").value should be(
      JObject(
        Map(
          "isError" -> JBool(true)
        )))
  }

  it should "parse a json object with null value" in {
    parser.run("""{"patronymic": null}""").value should be(
      JObject(
        Map(
          "patronymic" -> JNull
        )))
  }

  it should "parse a json object with object values" in {
    parser.run("""{"id": 1, "session": {"expiration": 1331}}""").value should be(
      JObject(
        Map(
          "id"      -> JNumber(1.0),
          "session" -> JObject(Map("expiration" -> JNumber(1331.0)))
        )))
  }

  it should "parse a json object with array values" in {
    parser.run("""{"country": "BY", "cities": ["Minsk", "Brest"]}""").value should be(
      JObject(
        Map(
          "country" -> JString("BY"),
          "cities"  -> JArray(IndexedSeq(JString("Minsk"), JString("Brest")))
        )))
  }
}
