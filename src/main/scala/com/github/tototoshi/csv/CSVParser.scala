/*
* Copyright 2013 Toshiyuki Takahashi
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package com.github.tototoshi.csv

import scala.annotation.switch

object CSVParser {

  private type State = Int
  private final val OutOfQuotes = 0
  private final val InQuotes = 1

  /**
   * {{{
   * scala> com.github.tototoshi.csv.CSVParser.parse("a,b,c", '\\', ',', '"')
   * res0: Option[List[String]] = Some(List(a, b, c))
   *
   * scala> com.github.tototoshi.csv.CSVParser.parse("\"a\",\"b\",\"c\"", '\\', ',', '"')
   * res1: Option[List[String]] = Some(List(a, b, c))
   * }}}
   */
  def parse(input: String, escapeChar: Char, delimiter: Char, quoteChar: Char): Option[List[String]] = {
    def parseWithState(toParse: List[Char], state: State = OutOfQuotes, field: String = "", parsed: List[String] = List()): Option[List[String]] = (toParse, state) match {
      case (Nil, InQuotes) => None
      case (Nil, OutOfQuotes) => Some(parsed.reverse)
      case (`escapeChar` :: `quoteChar` :: cs, InQuotes) => {
//                println("escape + quote")
        parseWithState(cs, InQuotes, field + quoteChar, parsed)
      }
      case (`escapeChar` :: other :: cs, InQuotes) if escapeChar == quoteChar && ("\n\u2028\u2029\u0085" + delimiter).contains(other) => {
//                println("escape=quote + end of cell")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (`escapeChar` :: '\r' :: '\n' :: cs, InQuotes) if escapeChar == quoteChar => {
//        println("quote at end of windows line")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (`escapeChar` :: other :: cs, InQuotes) => {
//                println("escape + other")
        parseWithState(cs, InQuotes, field + escapeChar + other, parsed)
      }
      case (`escapeChar` :: cs, InQuotes) if quoteChar == escapeChar => {
//        println("escape=quote + nil")
        throw new MalformedCSVException("quote ended before end of cell: " + cs)
      }
      case (`escapeChar` :: Nil, _) => {
//                println("escape + nil")
        throw new MalformedCSVException("escape char at end of line: " + input)
      }
      case (`quoteChar` :: cs, OutOfQuotes) if field.isEmpty => {
//                println("quote at start of field")
        parseWithState(cs, InQuotes, field, parsed)
      }
      case (`quoteChar` :: cs, OutOfQuotes) => {
                // println("quote in middle of field")
        throw new MalformedCSVException("quote start after start of cell: " + input)
      }
      case (`quoteChar` :: Nil, InQuotes) => {
                // println("quote at end of list")
        parseWithState(Nil, OutOfQuotes, field, parsed)
      }
      case (`quoteChar` :: other :: cs, InQuotes) if ("\n" + delimiter).contains(other) => {
                // println("quote at end of line")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (`quoteChar` :: '\r' :: '\n' :: cs, InQuotes) => {
                // println("quote at end of windows line")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (`quoteChar` :: cs, InQuotes) => {
                // println("quote in middle of cell")
        throw new MalformedCSVException("quote ended before end of cell: " + input)
      }
      case (`delimiter` :: cs, InQuotes) => {
                // println("delimiter in quotes")
        parseWithState(cs, InQuotes, field + delimiter, parsed)
      }
      case (`delimiter` :: cs, OutOfQuotes) => {
                // println("delimiter out of quotes")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (c :: cs, OutOfQuotes) if "\n".contains(c) => {
                // println("endOfLine out of quotes")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case ('\r' :: '\n' :: cs, OutOfQuotes) => {
                // println("Windows end of line out of quotes")
        parseWithState(cs, OutOfQuotes, "", field :: parsed)
      }
      case (c :: cs, _) => {
                // println("normal character")
        parseWithState(cs, state, field + c, parsed)
      }
    }

    parseWithState(input.toList)
  }

}

class CSVParser(format: CSVFormat) {

  def parseLine(input: String): Option[List[String]] = {
    val parsedResult = CSVParser.parse(input, format.escapeChar, format.delimiter, format.quoteChar)
    if (parsedResult == Some(List("")) && format.treatEmptyLineAsNil) Some(Nil)
    else parsedResult
  }

}
