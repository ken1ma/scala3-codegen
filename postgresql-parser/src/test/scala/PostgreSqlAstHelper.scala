package jp.ken1ma.postgresql
package parser

import PostgreSqlAst.*

/** Relaxed AST builders for tests */
object PostgreSqlAstHelper:
  implicit class StringInterpolator(private val sc: StringContext) extends AnyVal:
    protected def preBodySuc(text: String): (String, String, String) = {
      val pre  = text.takeWhile(_.isWhitespace)
      val body = text.drop(pre.length).takeWhile(!_.isWhitespace)
      val suc  = text.drop(pre.length + body.length)
      require(body.nonEmpty, s"body must not be empty: $text")
      (pre, body, suc)
    }

    def ws(args: Any*): Whitespace =
      val text = StringContext.standardInterpolator(StringContext.processEscapes, args, sc.parts)
      require(text.forall(_.isWhitespace), text)
      Whitespace(text)

    def token(args: Any*): Token =
      val text = StringContext.standardInterpolator(StringContext.processEscapes, args, sc.parts)
      val (pre, body, suc) = preBodySuc(text)
      Token(
        if pre.nonEmpty then Seq(Whitespace(pre)) else Nil,
        body,
        if suc.nonEmpty then Seq(Whitespace(suc)) else Nil,
      )

    def ident(args: Any*): Ident =
      val text = StringContext.standardInterpolator(StringContext.processEscapes, args, sc.parts)
      val (pre, body, suc) = preBodySuc(text)
      Ident(
        if pre.nonEmpty then Seq(Whitespace(pre)) else Nil,
        body,
        false,
        if suc.nonEmpty then Seq(Whitespace(suc)) else Nil,
      )

    def intLit(args: Any*): IntLit =
      val text = StringContext.standardInterpolator(StringContext.processEscapes, args, sc.parts)
      val (pre, body, suc) = preBodySuc(text)
      IntLit(
        if pre.nonEmpty then Seq(Whitespace(pre)) else Nil,
        body,
        if suc.nonEmpty then Seq(Whitespace(suc)) else Nil,
      )

    def op(args: Any*): Op =
      val text = StringContext.standardInterpolator(StringContext.processEscapes, args, sc.parts)
      val (pre, body, suc) = preBodySuc(text)
      Op(
        if pre.nonEmpty then Seq(Whitespace(pre)) else Nil,
        body,
        if suc.nonEmpty then Seq(Whitespace(suc)) else Nil,
      )
