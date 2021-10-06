package jp.ken1ma.postgresql
package parser

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.parse.{Parser => P, Parser0 => P0}

import PostgreSqlAst._

class PostgreSqlParserSuite extends munit.FunSuite:
  val parser = PostgreSqlParser()
  import parser._

  test("whitespace") {
    assertEquals(whitespace.parseAll(" "), Right(Whitespace(" ")))
    assertEquals(whitespace.parseAll("\t\n \r\n \r "), Right(Whitespace("\t\n \r\n \r ")))
  }

  test("whitespaceSingleLine") {
    assertEquals(whitespaceSingleLine.parse(" \n"), Right(("\n", Whitespace(" "))))
  }

  test("lineEnd") {
    assertEquals(lineEnd.parseAll("\n"), Right(Some("\n")))
    assertEquals(lineEnd.parseAll("\r\n"), Right(Some("\r\n")))
    assertEquals(lineEnd.parseAll("\r"), Right(Some("\r")))
    assertEquals(lineEnd.parseAll(""), Right(None))
  }

  test("lineComment") {
    assertEquals(lineComment.parseAll("--"), Right(LineComment("", None)))
    assertEquals(lineComment.parseAll("-- foo"), Right(LineComment(" foo", None)))
    assertEquals(lineComment.parseAll("--bar "), Right(LineComment("bar ", None)))
    assertEquals(lineComment.parseAll("-- コメント \n"), Right(LineComment(" コメント ", Some("\n"))))
  }

  test("blockComment.unnested") {
    assertEquals(blockComment.parseAll("/**/"), Right(BlockComment("")))
    assertEquals(blockComment.parseAll("/* */"), Right(BlockComment(" ")))
    assertEquals(blockComment.parseAll("/***/"), Right(BlockComment("*")))
    assertEquals(blockComment.parseAll("/*  */"), Right(BlockComment("  ")))
    assertEquals(blockComment.parseAll("/****/"), Right(BlockComment("**")))
    assertEquals(blockComment.parseAll("/* * */"), Right(BlockComment(" * ")))
    assertEquals(blockComment.parseAll("/*****/"), Right(BlockComment("***")))
    assertEquals(blockComment.parseAll("/* comment1\nコメント2 */"), Right(BlockComment(" comment1\nコメント2 ")))
    assertEquals(blockComment.parseAll("/* comment1\nコメント2\r\r\nCOMMENT3 */"), Right(BlockComment(" comment1\nコメント2\r\r\nCOMMENT3 ")))
  }

  test("blockComment.nested") {
    assertEquals(blockComment.parseAll("/*/**/*/"), Right(BlockComment("/**/")))
    assertEquals(blockComment.parseAll("/*/* */*/"), Right(BlockComment("/* */")))
    assertEquals(blockComment.parseAll("/*/***/*/"), Right(BlockComment("/***/")))
    assertEquals(blockComment.parseAll("/*/* /**/*/*/"), Right(BlockComment("/* /**/*/")))
    assertEquals(blockComment.parseAll("/*/*/**/ */*/"), Right(BlockComment("/*/**/ */")))
    assertEquals(blockComment.parseAll("/*/* /**/ */*/"), Right(BlockComment("/* /**/ */")))
    assertEquals(blockComment.parseAll("/*/**//**/*/"), Right(BlockComment("/**//**/")))
    assertEquals(blockComment.parseAll("/* /* */ /* */ */"), Right(BlockComment(" /* */ /* */ ")))
  }

  test("token.single") {
    assertEquals(t("SELECT").parseAll("SELECT"), Right(Token("SELECT")))
    assertEquals(t("SELECT").parseAll("select"), Right(Token("select")))
    assertEquals(t("SELECT").parseAll(" Select"), Right(Token(Seq(Whitespace(" ")), "Select")))
    assertEquals(t("SELECT").parseAll("sElEcT "), Right(Token("sElEcT", Seq(Whitespace(" ")))))
    assertEquals(t("SELECT").parseAll(" SelecT "), Right(Token(Seq(Whitespace(" ")), "SelecT", Seq(Whitespace(" ")))))
  }

  test("token.multiple") {
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("SELECT INTO"),
        Right((Token("SELECT"), Token(Seq(Whitespace(" ")), "INTO"))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" SELECT INTO "),
        Right((Token(Seq(Whitespace(" ")), "SELECT"), Token(Seq(Whitespace(" ")), "INTO", Seq(Whitespace(" "))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/SELECT/*b*/INTO/*c*/"),
        Right((Token(Seq(BlockComment("a")), "SELECT"), Token(Seq(BlockComment("b")), "INTO", Seq(BlockComment("c"))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/ select /*b*/into--c"),
        Right((Token(Seq(BlockComment("a"), Whitespace(" ")), "select"), Token(Seq(Whitespace(" "), BlockComment("b")), "into", Seq(LineComment("c", None))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" /*a*/ SELECT /*b*/ INTO /*c*/ "),
        Right((Token(Seq(Whitespace(" "), BlockComment("a"), Whitespace(" ")), "SELECT"), Token(Seq(Whitespace(" "), BlockComment("b"), Whitespace(" ")), "INTO", Seq(Whitespace(" "), BlockComment("c"), Whitespace(" "))))))
  }

  test("ident") {
    assertEquals(ident.parseAll("t0"), Right(Ident("t0")))
    assertEquals(ident.parseAll("\"t0\""), Right(Ident(Nil, "t0", true)))
    assertEquals(ident.parseAll(" t1 \n"), Right(Ident(Seq(Whitespace(" ")), "t1", false, Seq(Whitespace(" \n")))))
    assertEquals(ident.parseAll(" /*c0*/  /* c1 */ t2 /*c2*/  --c3"), Right(Ident(
      Seq(Whitespace(" "), BlockComment("c0"), Whitespace("  "), BlockComment(" c1 "), Whitespace(" ")),
      "t2", false,
      Seq(Whitespace(" "), BlockComment("c2"), Whitespace("  "), LineComment("c3", None)),
    )))
  }

  test("booleanLit") {
    assertEquals(booleanLit.parseAll("FALSE"), Right(BooleanLit(false)))
    assertEquals(booleanLit.parseAll("true" ), Right(BooleanLit("true")))
  }

  test("integerLit") {
    assertEquals(integerLit.parseAll("0"), Right(IntegerLit(0)))
    assertEquals(integerLit.parseAll("1234567890"), Right(IntegerLit(1234567890)))
    assertEquals(integerLit.parseAll("-1"), Right(IntegerLit(-1)))
  }

  test("stringLit") {
    assertEquals(stringLit.parseAll("''"), Right(StringLit("")))
    assertEquals(stringLit.parseAll(" '文字列' "), Right(StringLit(Seq(Whitespace(" ")), "文字列", Seq(Whitespace(" ")))))
  }

  test("op") {
    assertEquals(op.parseAll("+"), Right(Op("+")))
    assertEquals(op.parseAll("-"), Right(Op("-")))
    assertEquals(op.parseAll("*"), Right(Op("*")))
    assertEquals(op.parseAll("/"), Right(Op("/")))

    assertEquals(op.parseAll("<" ), Right(Op("<" )))
    assertEquals(op.parseAll("<="), Right(Op("<=")))
    assertEquals(op.parseAll(">" ), Right(Op(">" )))
    assertEquals(op.parseAll(">="), Right(Op(">=")))
    assertEquals(op.parseAll("=" ), Right(Op("=" )))

    assertEquals(op.parseAll("~"), Right(Op("~")))
    assertEquals(op.parseAll("!"), Right(Op("!")))
    assertEquals(op.parseAll("@"), Right(Op("@")))
    assertEquals(op.parseAll("#"), Right(Op("#")))
    assertEquals(op.parseAll("%"), Right(Op("%")))
    assertEquals(op.parseAll("^"), Right(Op("^")))
    assertEquals(op.parseAll("&"), Right(Op("&")))
    assertEquals(op.parseAll("|"), Right(Op("|")))
    assertEquals(op.parseAll("`"), Right(Op("`")))
    assertEquals(op.parseAll("?"), Right(Op("?")))

    // 4.1.3 @- is an allowed operator name, but *- is not
    assertEquals(op.parseAll("@-"), Right(Op("@-")))
    assert(op.parse("*-").isLeft)
  }

  test("nullability") {
    assertEquals(nullability.parseAll("NULL"), Right(Nullability(false)))
    assertEquals(nullability.parseAll("NOT NULL"), Right(Nullability(true)))
  }

