package jp.ken1ma.postgresql
package parser

import java.lang.Character.{isJavaIdentifierStart, isJavaIdentifierPart}

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.parse.{Parser => P, Parser0 => P0}

import PostgreSqlAst._

class PostgreSqlParser:
  /** Helper methods */
  extension (str: String)
    inline def except(those: String): String = str.filter(!those.contains(_))
    inline def isOneOf(those: String*): Boolean = those.contains(str)
/*
  extension (whitespaceOrComment: WhitespaceOrComment)
    inline def containsNewLine: Boolean = whitespaceOrComment.body.exists(ch => ch == '\n' || ch == '\r')
  extension [A, AA <: A](tuple: Tuple2[A, AA])
    inline def toList: List[A] = List(tuple._1, tuple._2)
*/
/*unreliable
  def log[A](label: String, p: P[A]): P[A] = P.index.with1.flatMap { index =>
    println(s"$index: trying: $label")
    p.map { result =>
      println(s"$index: parsed: $label: $result")
      result
    }
  }
  def log[A](label: String, p: P0[A]): P0[A] = P.index.flatMap { index =>
    println(s"$index: trying: $label")
    p.?.flatMap { _ match
      case Some(result) =>
        println(s"$index: parsed: $label: $result")
        P.pure(result)
      case None =>
        println(s"$index: failed: $label")
        P.fail
    }
  }
*/

  val whitespace: P[Whitespace] = P.charsWhile(_.isWhitespace).map(Whitespace(_)).withContext("Whitespace") // Unicode space characters (not just ASCII)
  val whitespaceSingleLine: P[Whitespace] = P.charsWhile(ch => ch.isWhitespace && ch != '\n' && ch != '\r')
      .map(Whitespace(_)).withContext("WhitespaceSingleLine")
  val newLine: P[String] = (P.string("\n") | P.string("\r\n") | P.string("\r")).string.withContext("NewLine") // P.stringIn finds longest match, which we don't need here
  val lineEnd: P0[Option[String]] = (newLine.map(Some(_)) | P.end.as(None)).withContext("LineEnd")

  // 4.1.5. Comments
  val lineComment: P[LineComment] = (P.string("--") *> P.charsWhile0(ch => ch != '\n' && ch != '\r') ~ lineEnd)
      .map(LineComment(_, _)).withContext("LineComment")

  val blockComment: P[BlockComment] = {
    val block: P[String] = P.recursive { block => // block comment can nest
      val start = P.string("/*")
      val end   = P.string("*/")
      val text = (P.not(start | end).with1 ~ P.anyChar).rep
      start *> (text | block).rep0.string <* end
    }
    block.map(BlockComment(_))
  }.withContext("BlockComment")

  /** whitespaces/comments preceeding a token */
  val pre: P0[List[WhitespaceOrComment]] = (whitespace | lineComment | blockComment).rep0

  /** whitespaces/comments succeeding a token */
  val suc: P0[List[WhitespaceOrComment]] = {
    // succeeding whitespaces/comments are consumed only when there are only whitespaces/comments up to a newline or EOF
    val blockComments: P0[List[WhitespaceOrComment]] = (whitespaceSingleLine.?.with1 ~ blockComment).backtrack
        .map(_.toList :+ _).rep0.map(_.flatten)
    val lineCommentOrEnd: P0[Option[LineComment | Whitespace]] = (lineComment.map(Some(_)) | lineEnd.map(_.map(Whitespace(_))))
    val upToLineEnd: P0[List[WhitespaceOrComment]] = (whitespaceSingleLine.? ~ lineCommentOrEnd).backtrack
        .map { (whitespaceOpt, endOpt) => endOpt match
          case Some(comment: LineComment) => whitespaceOpt.toList :+ comment
          case Some(lineEnd: Whitespace) => List(Whitespace(whitespaceOpt.map(_.body).mkString + lineEnd.body))
          case None => whitespaceOpt.toList
        }
    (blockComments ~ upToLineEnd).map(_ ++ _).backtrack
  }
  def preBodySuc[A](body: P[A]): P[(List[WhitespaceOrComment], A, List[WhitespaceOrComment])] = (pre.with1 ~ body ~ suc.?).backtrack
      .map { case ((pre, body), sucOpt) => (pre, body, sucOpt.getOrElse(Nil)) }

  /** Token */
  def t(text: String): P[Token] = preBodySuc(P.ignoreCase(text).string).map(Token(_, _, _))

  // ident
  val unquotedIdent: P[String] = (P.charWhere(isJavaIdentifierStart) ~ P.charsWhile0(isJavaIdentifierPart)).string
  val quotedIdentPart = P.charWhere(_ != '"') | P.string("\"\"")
  val quotedIdent: P[String] = P.char('"') *> quotedIdentPart.rep0.string <* P.char('"')
  val ident: P[Ident] = preBodySuc(
    unquotedIdent.map((_, false)) |
    quotedIdent  .map((_, true ))
  ).map { case (pre, (body, quoted), suc) => Ident(pre, body, quoted, suc) }

  // 4.1.3. Operators
  val op: P[Op] = {
    // An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list
    val opChars                  = "+-*/<>=~!@#%^&|`?"

    // -- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment
    val minus = P.char('-') <* P.peek(P.not(P.char('-'))).backtrack
    val slash = P.char('/') <* P.peek(P.not(P.char('*'))).backtrack

    // A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters
    val plusMinusLastEnableChars =        "~!@#%^&|`?"
    val plusMinusLastDisallowed = P.charIn(opChars.except("+-/")) | slash |
        (P.char('+') <* P.peek(P.not(P.charIn(opChars.except("+-")) | P.end))).backtrack |
        (P.char('-') <* P.peek(P.not(P.charIn(opChars.except("+-")) | P.end))).backtrack
    val plusMinusLastAllowed = P.charIn(opChars.except("-/")) | minus | slash
    val multiCharOp =
        ((P.charIn(opChars.except(plusMinusLastEnableChars + "-/")) | minus | slash).rep0.with1 ~ P.charIn(plusMinusLastEnableChars) ~ plusMinusLastAllowed.rep0).backtrack |
        plusMinusLastDisallowed.rep(2).backtrack

    val singleCharOp =
        (P.charIn(opChars) <* P.peek(P.not(P.charIn(opChars)))).backtrack

    preBodySuc((multiCharOp | singleCharOp).string).map(Op(_, _, _))
  }

  // Literal
  val booleanLit: P[BooleanLit]= preBodySuc((P.ignoreCase("TRUE") | P.ignoreCase("FALSE")).string).map(BooleanLit(_, _, _))

  val integerBody: P[String] = (P.charIn("+-").?.with1 ~ P.charIn('0' to '9').rep).string.backtrack
  val integerLit: P[IntegerLit]= preBodySuc(integerBody.string).map(IntegerLit(_, _, _))

  val stringLitBody: P0[String] = (P.charWhere(_ != '\'') | P.string("''")).rep0.string
  val stringLit: P[StringLit] = preBodySuc(P.char('\'') *> stringLitBody <* P.char('\'')).map(StringLit(_, _, _))

  val lit: P[Lit] = booleanLit | integerLit | stringLit

  // Expression
  val expr: P[Expr] = lit | otherBinOp /*|
      (t("(") ~ expr ~ t(")")).map(Expr.Paren(_, _, _))*/

  // 4.1.6. Operator Precedence
  val unary = expr
  val exp: P[Expr] = (unary ~ (op.filter(_.body.isOneOf("^"          )) ~ unary).rep0).map(binOpFoldLeft)
  val mul: P[Expr] = (exp   ~ (op.filter(_.body.isOneOf("+", "-", "%")) ~ exp  ).rep0).map(binOpFoldLeft)
  val add: P[Expr] = (mul   ~ (op.filter(_.body.isOneOf("+", "-"     )) ~ mul  ).rep0).map(binOpFoldLeft)
  val otherBinOp: P[Expr] = (add ~ (op ~ add                                   ).rep0).map(binOpFoldLeft)
  def binOpFoldLeft(left: Expr, opRights: Seq[(Op, Expr)]): Expr =
      opRights.foldLeft(left)((left, opRight) => BinOp(left, opRight._1, opRight._2))

  val nullability = (t("NOT").? ~ t("NULL")).map(Nullability(_, _)).backtrack
/*
  def createTable: P[CreateTable] = {
    val columnDef = name ~ tpe ~ arraySpec.? ~ columnConstraint.rep0
    val entry = columnDef | tableConstraint
    (t("CREATE") ~ t("TABLE") ~ ident ~ t("(") ~ entry ~ t(")")
  }
*/
