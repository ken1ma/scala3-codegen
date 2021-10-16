package jp.ken1ma.postgresql
package parser

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.parse.{Parser, LocationMap}, Parser.{Error, Expectation}

object CatsParseHelper:
  class ErrorPrettyPrint(str: String, fileName: String, lineOffset: Int = 1):
    val locMap = LocationMap(str) // cats-parse 0.3.4: only handles '\n' new lines

    def prettyPrint(exp: Parser.Expectation): String =
      val (loc, indicated) = locMap.toLineCol(exp.offset) match
        case Some((line, col)) =>
          val lineText = locMap.getLine(line).mkString
          val indicator = lineText.take(col).map(_ => " ").mkString + "^" // TODO map to two spaces for CJK characters
          (s"$fileName: ${line + 1}:${col + 1}: ${ErrorPrettyPrint.prettyPrint(exp)}", s"$lineText\n$indicator")
        case None =>
          (s"$fileName: EOF: ${ErrorPrettyPrint.prettyPrint(exp)}", "")
      s"$loc\n$indicated"

    def prettyPrint(err: Parser.Error): String =
      err.expected.map(prettyPrint).toList.mkString("\n")

  object ErrorPrettyPrint:
    def prettyPrint(exp: Parser.Expectation): String =
      import Expectation._
      exp match
        case OneOfStr(offset, strs) =>
          s"Expected one of strings: ${strs.mkString(", ")}"

        case InRange(offset, lower, upper) =>
          if (lower == upper) then s"Expected character: $lower"
          else s"Expected character in range: $lower ~ $upper"

        case StartOfString(offset) =>
          "Expected the beginning of the file"

        case EndOfString(offset, len) =>
          "Expected the end of the file"

        case Length(offset, expected, actual) =>
          s"Expected $expected characters but got $actual"

        case ExpectedFailureAt(offset, matched) =>
          s"Expected failure: $matched"

        case Fail(offset) =>
          s"Failed"

        case FailWith(offset, message) =>
          s"Failed: $message"

        case WithContext(contextStr, exp) =>
          s"Expected: $contextStr"
