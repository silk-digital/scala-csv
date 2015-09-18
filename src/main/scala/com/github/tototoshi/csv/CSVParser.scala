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
  private final val End = 2

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
      case (Nil, End) => Some(parsed.reverse)
      case (_, End) => throw new MalformedCSVException("line ended before end of line: " + input)
      case (Nil, _) => None
      case (`escapeChar` :: next :: cs, state) => parseWithState(cs, state, field + next, parsed)
      case (`escapeChar` :: Nil, _) => throw new MalformedCSVException("escape char at end of line: " + input)
      case (`quoteChar` :: cs, OutOfQuotes) if field.isEmpty => parseWithState(cs, InQuotes, field, parsed)
      case (`quoteChar` :: cs, OutOfQuotes) => throw new MalformedCSVException("quote start after start of cell: " + input)
      case (`quoteChar` :: `delimiter` :: cs, InQuotes) => parseWithState(cs, OutOfQuotes, "", field :: parsed)
      case (`quoteChar` :: cs, InQuotes) => throw new MalformedCSVException("quote ended before end of cell: " + input)
      case (`delimiter` :: cs, InQuotes) => {
        // warning unescaped delimiter
        parseWithState(cs, InQuotes, field + delimiter, parsed)
      }
      case (`delimiter` :: cs, OutOfQuotes) => parseWithState(cs, OutOfQuotes, "", field :: parsed)
      case (c :: cs, OutOfQuotes) if "\n\u2028\u2029\u0085".contains(c) => parseWithState(cs, End, "", field :: parsed)
      case ('\r' :: '\n' :: cs, OutOfQuotes) => parseWithState(cs, End, "", field :: parsed)
      case (c :: cs, state) => parseWithState(cs, state, field + c, parsed)
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
