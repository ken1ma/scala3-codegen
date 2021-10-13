package jp.ken1ma.postgresql
package parser

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.parse.{Parser => P, Parser0 => P0}

import PostgreSqlAst.*

class PostgreSqlParserSuite extends munit.FunSuite:
  val parser = PostgreSqlParser()
  import parser._

  val space1 = Whitespace(" ")
  val space1s = Seq(space1)

  val newLine1 = Whitespace("\n")
  val newLine1s = Seq(newLine1)

  val space2 = Whitespace("  ")
  val space2s = Seq(space2)

  // Use this when the diff context is too short
  def assertEqualsLong[A](obtained: A, expected: A) = {
    println(s"obtained = $obtained")
    println(s"expected = $expected")
    assertEquals(obtained, expected)
  }

  test("whitespace") {
    assertEquals(whitespace.parseAll(" "), Right(space1))
    assertEquals(whitespace.parseAll("\t\n \r\n \r "), Right(Whitespace("\t\n \r\n \r ")))
  }

  test("whitespaceSingleLine") {
    assertEquals(whitespaceSingleLine.parse(" \n"), Right(("\n", space1)))
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
    assertEquals(t("SELECT").parseAll(" Select"), Right(Token(space1s, "Select")))
    assertEquals(t("SELECT").parseAll("sElEcT "), Right(Token("sElEcT", space1s)))
    assertEquals(t("SELECT").parseAll(" SelecT "), Right(Token(space1s, "SelecT", space1s)))
  }

  test("token.multiple") {
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("SELECT INTO"),
        Right((Token("SELECT"), Token(space1s, "INTO"))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" SELECT INTO "),
        Right((Token(space1s, "SELECT"), Token(space1s, "INTO", space1s))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/SELECT/*b*/INTO/*c*/"),
        Right((Token(Seq(BlockComment("a")), "SELECT"), Token(Seq(BlockComment("b")), "INTO", Seq(BlockComment("c"))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/ select /*b*/into--c"),
        Right((Token(Seq(BlockComment("a"), space1), "select"), Token(Seq(space1, BlockComment("b")), "into", Seq(LineComment("c", None))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" /*a*/ SELECT /*b*/ INTO /*c*/ "),
        Right((Token(Seq(space1, BlockComment("a"), space1), "SELECT"), Token(Seq(space1, BlockComment("b"), space1), "INTO", Seq(space1, BlockComment("c"), space1)))))
  }

  test("ident") {
    assertEquals(ident.parseAll("t0"), Right(Ident("t0")))
    assertEquals(ident.parseAll("\"t0\""), Right(Ident(Nil, "t0", true)))
    assertEquals(ident.parseAll(" t1 \n"), Right(Ident(space1s, "t1", false, Seq(Whitespace(" \n")))))
    assertEquals(ident.parseAll(" /*c0*/  /* c1 */ t2 /*c2*/  --c3"), Right(Ident(
      Seq(space1, BlockComment("c0"), Whitespace("  "), BlockComment(" c1 "), space1),
      "t2", false,
      Seq(space1, BlockComment("c2"), Whitespace("  "), LineComment("c3", None)),
    )))
  }

  test("booleanLit") {
    assertEquals(booleanLit.parseAll("FALSE"), Right(BooleanLit(false)))
    assertEquals(booleanLit.parseAll("true" ), Right(BooleanLit("true")))
  }

  test("intLit") {
    assertEquals(intLit.parseAll("0"), Right(IntLit(0)))
    assertEquals(intLit.parseAll("1234567890"), Right(IntLit(1234567890)))
  }

  test("stringLit") {
    assertEquals(stringLit.parseAll("''"), Right(StringLit("")))
    assertEquals(stringLit.parseAll(" '文字列' "), Right(StringLit(space1s, "文字列", space1s)))
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

  test("varchar") {
    assertEquals(sqlType.parseAll("varchar"), Right(varchar(Seq(Ident("varchar")))))
    assertEquals(sqlType.parseAll("character VARYING(3)" ), Right(varchar(Seq(Ident("character"), Ident(space1s, "VARYING")), Some(LenArg(Token("("), IntLit(3), Token(")"))))))
  }

  test("expr") {
    assertEquals(expr.parseAll("2+3"),   Right(BinOp(IntLit(2), Op("+"), IntLit(3))))
    assertEquals(expr.parseAll(" 2 - 3 + 5 "), Right(BinOp(BinOp(IntLit(space1s, 2), Op(space1s, "-"), IntLit(space1s, 3)), Op(space1s, "+"), IntLit(space1s, 5, space1s))))
    assertEquals(expr.parseAll("2*3/5"), Right(BinOp(BinOp(IntLit(2), Op("*"), IntLit(3)), Op("/"), IntLit(5))))
    assertEquals(expr.parseAll("2+3%5"), Right(BinOp(IntLit(2), Op("+"), BinOp(IntLit(3), Op("%"), IntLit(5)))))

    assertEquals(expr.parseAll("2 BETWEEN 3 AND 5"), Right(BetweenOp(IntLit(2), Op(space1s, "BETWEEN"), IntLit(space1s, 3), Op(space1s, "AND"), IntLit(space1s, 5))))
    assertEquals(expr.parseAll("2 NoT bETWEEn 3 anD 5"), Right(BetweenOp(IntLit(2), Some(Op(space1s, "NoT")), Op(space1s, "bETWEEn"), IntLit(space1s, 3), Op(space1s, "anD"), IntLit(space1s, 5))))

    assertEquals(expr.parseAll("2+3=5"), Right(BinOp(BinOp(IntLit(2), Op("+"), IntLit(3)), Op("="), IntLit(5))))
    assertEquals(expr.parseAll("2<=3*5"), Right(BinOp(IntLit(2), Op("<="), BinOp(IntLit(3), Op("*"), IntLit(5)))))
    assertEquals(expr.parseAll("2<3 AND 5 OR NOT 7"), Right(BinOp(BinOp(BinOp(IntLit(2), Op("<"), IntLit(3)), Op(space1s, "AND"), IntLit(space1s, 5)), Op(space1s, "OR"), UnaryOp(Op(space1s, "NOT"), IntLit(space1s, 7)))))
  }

  test("nullability") {
    assertEquals(nullability.parseAll("NULL"), Right(Nullability(false)))
    assertEquals(nullability.parseAll("NOT NULL"), Right(Nullability(true)))
  }

  test("CREATE TABLE") {
    assertEquals(createTable.parseAll("""
        CREATE TABLE Foo(a integer)
      """.trim), Right(
      CreateTable(Token("CREATE"), Token(space1s, "TABLE"), Ident(space1s, "Foo"), Token("("), SeqTokenSep(
        Column(Ident("a"), integer(Ident(space1s, "integer"))),
      ), Token(")"))
    ))

    assertEquals(createTable.parseAll("""
        |CREATE TABLE Foo(
        |  id integer,
        |  "score" numeric(3, 2),
        |  PRIMARY KEY(id)
        |)
        """.stripMargin.trim), Right(
      CreateTable(Token("CREATE"), Token(space1s, "TABLE"), Ident(space1s, "Foo"), Token("(", newLine1s), SeqTokenSep(Seq(
        Column(Ident(space2s, "id"), integer(Ident(space1s, "integer"))),
        Column(Ident(space2s, "score", true), numeric(Ident(space1s, "numeric"), Some(NumericArgs(Token("("), IntLit(3), Some(NumericScale(Token(","), IntLit(space1s, 2))), Token(")"))))),
        PrimaryKey(Token(space2s, "PRIMARY"), Token(space1s, "KEY"), Token("("), SeqTokenSep(
          Ident("id")
        ), Token(")", newLine1s)),
      ), Seq(
        Token(",", newLine1s),
        Token(",", newLine1s),
      )), Token(")"))
    ))
  }

