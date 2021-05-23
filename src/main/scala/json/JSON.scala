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

sealed trait JSON

case object JNull                                extends JSON
final case class JNumber(get: Double)            extends JSON
final case class JString(get: String)            extends JSON
final case class JBool(get: Boolean)             extends JSON
final case class JArray(get: IndexedSeq[JSON])   extends JSON
final case class JObject(get: Map[String, JSON]) extends JSON

object JSON {
  def parser[Parser[+ _]](implicit P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def jNull: Parser[JSON]      = string("null").as(JNull)
    def jString: Parser[JString] = token(quoted) ^^ JString
    def jNumber: Parser[JNumber] = double ^^ JNumber
    def jBool: Parser[JBool]     = string("true").as(JBool(true)) | string("false").as(JBool(false))

    def lit: Parser[JSON] = jNull | jString | jNumber | jBool

    def keyVal: Parser[(String, JSON)] = token(quoted) ** (token(char(':')) *> value)

    def value: Parser[JSON] = obj | array | lit

    def array: Parser[JArray] =
      surrounded(token(char('[')), token(char(']'))) {
        value.split(token(char(','))) | whitespace.*.as(Nil)
      } ^^ (values => JArray(values.toIndexedSeq))

    def obj: Parser[JObject] =
      surrounded(token(char('{')), token(char('}'))) {
        keyVal.split(token(char(','))) | whitespace.*.as(Nil)
      } ^^ (pairs => JObject(Map.from(pairs)))

    (whitespace.* *> (array | obj)) <* eoi
  }
}
