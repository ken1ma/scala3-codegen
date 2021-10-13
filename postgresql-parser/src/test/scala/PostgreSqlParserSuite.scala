package jp.ken1ma.postgresql
package parser

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.parse.{Parser => P, Parser0 => P0}

import PostgreSqlAst.*
import PostgreSqlAstHelper.*

class PostgreSqlParserSuite extends munit.FunSuite:
  val parser = PostgreSqlParser()
  import parser._

  // Use this when the diff context is too short
  def assertEqualsLong[A](obtained: A, expected: A) = {
    println(s"obtained = $obtained")
    println(s"expected = $expected")
    assertEquals(obtained, expected)
  }

  test("whitespace") {
    assertEquals(whitespace.parseAll(" "), Right(ws" "))
    assertEquals(whitespace.parseAll("\t\n \r\n \r "), Right(ws"\t\n \r\n \r "))
  }

  test("whitespaceSingleLine") {
    assertEquals(whitespaceSingleLine.parse(" \n"), Right(("\n", ws" ")))
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
    assertEquals(t("SELECT").parseAll("SELECT"), Right(token"SELECT"))
    assertEquals(t("SELECT").parseAll("select"), Right(token"select"))
    assertEquals(t("SELECT").parseAll(" Select"), Right(token" Select"))
    assertEquals(t("SELECT").parseAll("sElEcT "), Right(token"sElEcT "))
    assertEquals(t("SELECT").parseAll(" SelecT "), Right(token" SelecT "))
  }

  test("token.multiple") {
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("SELECT INTO"),
        Right((token"SELECT", token" INTO")))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" SELECT INTO "),
        Right((token" SELECT", token" INTO ")))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/SELECT/*b*/INTO/*c*/"),
        Right((Token(Seq(BlockComment("a")), "SELECT"), Token(Seq(BlockComment("b")), "INTO", Seq(BlockComment("c"))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll("/*a*/ select /*b*/into--c"),
        Right((Token(Seq(BlockComment("a"), ws" "), "select"), Token(Seq(ws" ", BlockComment("b")), "into", Seq(LineComment("c", None))))))
    assertEquals((t("SELECT") ~ t("INTO")).parseAll(" /*a*/ SELECT /*b*/ INTO /*c*/ "),
        Right((Token(Seq(ws" ", BlockComment("a"), ws" "), "SELECT"), Token(Seq(ws" ", BlockComment("b"), ws" "), "INTO", Seq(ws" ", BlockComment("c"), ws" ")))))
  }

  test("ident") {
    assertEquals(ident.parseAll("t0"), Right(ident"t0"))
    assertEquals(ident.parseAll("\"t0\""), Right(Ident(Nil, "t0", true)))
    assertEquals(ident.parseAll(" t1 \n"), Right(ident" t1 \n"))
    assertEquals(ident.parseAll(" /*c0*/  /* c1 */ t2 /*c2*/  --c3"), Right(Ident(
      Seq(ws" ", BlockComment("c0"), ws"  ", BlockComment(" c1 "), ws" "),
      "t2", false,
      Seq(ws" ", BlockComment("c2"), ws"  ", LineComment("c3", None)),
    )))
  }

  test("booleanLit") {
    assertEquals(booleanLit.parseAll("FALSE"), Right(BooleanLit(false)))
    assertEquals(booleanLit.parseAll("true" ), Right(BooleanLit("true")))
  }

  test("intLit") {
    assertEquals(intLit.parseAll("0"), Right(intLit"0"))
    assertEquals(intLit.parseAll("1234567890"), Right(intLit"1234567890"))
  }

  test("stringLit") {
    assertEquals(stringLit.parseAll("''"), Right(StringLit("")))
    assertEquals(stringLit.parseAll(" '文字列' "), Right(StringLit(Seq(ws" "), "文字列", Seq(ws" "))))
  }

  test("op") {
    assertEquals(op.parseAll("+"), Right(op"+"))
    assertEquals(op.parseAll("-"), Right(op"-"))
    assertEquals(op.parseAll("*"), Right(op"*"))
    assertEquals(op.parseAll("/"), Right(op"/"))

    assertEquals(op.parseAll("<" ), Right(op"<" ))
    assertEquals(op.parseAll("<="), Right(op"<="))
    assertEquals(op.parseAll(">" ), Right(op">" ))
    assertEquals(op.parseAll(">="), Right(op">="))
    assertEquals(op.parseAll("=" ), Right(op"=" ))

    assertEquals(op.parseAll("~"), Right(op"~"))
    assertEquals(op.parseAll("!"), Right(op"!"))
    assertEquals(op.parseAll("@"), Right(op"@"))
    assertEquals(op.parseAll("#"), Right(op"#"))
    assertEquals(op.parseAll("%"), Right(op"%"))
    assertEquals(op.parseAll("^"), Right(op"^"))
    assertEquals(op.parseAll("&"), Right(op"&"))
    assertEquals(op.parseAll("|"), Right(op"|"))
    assertEquals(op.parseAll("`"), Right(op"`"))
    assertEquals(op.parseAll("?"), Right(op"?"))

    // 4.1.3 @- is an allowed operator name, but *- is not
    assertEquals(op.parseAll("@-"), Right(op"@-"))
    assert(op.parse("*-").isLeft)
  }

  test("varchar") {
    assertEquals(sqlType.parseAll("varchar"), Right(varchar(Seq(ident"varchar"))))
    assertEquals(sqlType.parseAll("character VARYING(3)" ), Right(varchar(Seq(ident"character", ident" VARYING"), Some(LenArg(token"(", intLit"3", token")")))))
  }

  test("expr") {
    assertEquals(expr.parseAll("2+3"),   Right(BinOp(intLit"2", op"+", intLit"3")))
    assertEquals(expr.parseAll(" 2 - 3 + 5 "), Right(BinOp(BinOp(intLit" 2", op" -", intLit" 3"), op" +", intLit" 5 ")))
    assertEquals(expr.parseAll("2*3/5"), Right(BinOp(BinOp(intLit"2", op"*", intLit"3"), op"/", intLit"5")))
    assertEquals(expr.parseAll("2+3%5"), Right(BinOp(intLit"2", op"+", BinOp(intLit"3", op"%", intLit"5"))))

    assertEquals(expr.parseAll("2 BETWEEN 3 AND 5"), Right(BetweenOp(intLit"2", op" BETWEEN", intLit" 3", op" AND", intLit" 5")))
    assertEquals(expr.parseAll("2 NoT bETWEEn 3 anD 5"), Right(BetweenOp(intLit"2", Some(op" NoT"), op" bETWEEn", intLit" 3", op" anD", intLit" 5")))

    assertEquals(expr.parseAll("2+3=5"), Right(BinOp(BinOp(intLit"2", op"+", intLit"3"), op"=", intLit"5")))
    assertEquals(expr.parseAll("2<=3*5"), Right(BinOp(intLit"2", op"<=", BinOp(intLit"3", op"*", intLit"5"))))
    assertEquals(expr.parseAll("2<3 AND 5 OR NOT 7"), Right(BinOp(BinOp(BinOp(intLit"2", op"<", intLit"3"), op" AND", intLit" 5"), op" OR", UnaryOp(op" NOT", intLit" 7"))))
  }

  test("nullability") {
    assertEquals(nullability.parseAll("NULL"), Right(Nullability(false)))
    assertEquals(nullability.parseAll("NOT NULL"), Right(Nullability(true)))
  }

  test("CREATE TABLE") {
    assertEquals(createTable.parseAll("""
        CREATE TABLE Foo(a integer)
      """.trim), Right(
      CreateTable(token"CREATE", token" TABLE", ident" Foo", token"(", SeqTokenSep(
        Column(ident"a", integer(ident" integer")),
      ), token")")
    ))

    assertEquals(createTable.parseAll("""
        |CREATE TABLE Foo(
        |  id integer,
        |  "score" numeric(3, 2),
        |  PRIMARY KEY(id)
        |)
        """.stripMargin.trim), Right(
      CreateTable(token"CREATE", token" TABLE", ident" Foo", token"(\n", SeqTokenSep(Seq(
        Column(ident"  id", integer(ident" integer")),
        Column(Ident(ws"  ", "score", true), numeric(ident" numeric", Some(NumericArgs(token"(", intLit"3", Some(NumericScale(token",", intLit" 2")), token")")))),
        PrimaryKey(token"  PRIMARY", token" KEY", token"(", SeqTokenSep(ident"id"), token")\n"),
      ), token",\n"), token")")
    ))
  }
