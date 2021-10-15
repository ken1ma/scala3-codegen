package jp.ken1ma.postgresql
package parser

import java.lang.Character.{isJavaIdentifierStart, isJavaIdentifierPart}

import cats.data.NonEmptyList
import cats.syntax.all.*
import cats.parse.{Parser => P, Parser0 => P0}

import PostgreSqlAst.*

class PostgreSqlParser:
  /** Helper methods */
  extension (str: String)
    inline def except(those: String): String = str.filter(!those.contains(_))
    inline def isOneOf(those: String*): Boolean = those.contains(str)
  extension (token: Token)
    inline def toIdent: Ident = Ident(token.pre, token.body, false, token.suc)
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

  // `(pa, pb, pc...).tupled` described in cats-parse/README.md results in Parser0
  // https://github.com/typelevel/cats-parse/issues/235
  def unnest[A, B, C               ](t:      ((A, B), C)                    ): (A, B, C               ) = (                                                                                                         t._1._1, t._1._2, t._2)
  def unnest[A, B, C, D            ](t:     (((A, B), C), D)                ): (A, B, C, D            ) = (                                                                                          t._1._1._1, t._1._1._2, t._1._2, t._2)
  def unnest[A, B, C, D, E         ](t:    ((((A, B), C), D), E)            ): (A, B, C, D, E         ) = (                                                                        t._1._1._1._1, t._1._1._1._2, t._1._1._2, t._1._2, t._2)
  def unnest[A, B, C, D, E, F      ](t:   (((((A, B), C), D), E), F)        ): (A, B, C, D, E, F      ) = (                                                   t._1._1._1._1._1, t._1._1._1._1._2, t._1._1._1._2, t._1._1._2, t._1._2, t._2)
  def unnest[A, B, C, D, E, F, G   ](t:  ((((((A, B), C), D), E), F), G)    ): (A, B, C, D, E, F, G   ) = (                           t._1._1._1._1._1._1, t._1._1._1._1._1._2, t._1._1._1._1._2, t._1._1._1._2, t._1._1._2, t._1._2, t._2)
  def unnest[A, B, C, D, E, F, G, H](t: (((((((A, B), C), D), E), F), G), H)): (A, B, C, D, E, F, G, H) = (t._1._1._1._1._1._1._1, t._1._1._1._1._1._1._2, t._1._1._1._1._1._2, t._1._1._1._1._2, t._1._1._1._2, t._1._1._2, t._1._2, t._2)

  //def unnest[A, B, C](t: (A, (B, C))): (A, B, C) = (t._1, t._2._1, t._2._2) // compiler error: Double definition with ((A, B), C)) (have the same type after erasure)
  def unnest[A, B, C, D](t: (A, (B, C), D)): (A, B, C, D) = (t._1, t._2._1, t._2._2, t._3)

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
  def preBodySuc[A](body: P[A]): P[(List[WhitespaceOrComment], A, List[WhitespaceOrComment])] =
      (pre.with1 ~ body ~ suc.?.map(_.getOrElse(Nil))).backtrack
      .map(unnest)

  /** Token */
  def t(text: String): P[Token] = preBodySuc(P.ignoreCase(text).string).map(Token(_, _, _))

  extension [A](p: P[A])
    def repTokenSep(sep: P[Token]): P[SeqTokenSep[A]] = (p ~ (sep ~ p).rep0).map(SeqTokenSep(_, _))
    def repTokenSep(sep: String): P[SeqTokenSep[A]] = repTokenSep(t(sep))

  // ident
  val unquotedIdent: P[String] = (P.charWhere(isJavaIdentifierStart) ~ P.charsWhile0(isJavaIdentifierPart)).string
  val quotedIdentPart = P.charWhere(_ != '"') | P.string("\"\"")
  val quotedIdent: P[String] = P.char('"') *> quotedIdentPart.rep0.string <* P.char('"')
  val ident: P[Ident] = preBodySuc(
    unquotedIdent.map((_, false)) |
    quotedIdent  .map((_, true ))
  ).map(unnest).map(Ident(_, _, _, _))

  // Literal
  val booleanLit: P[BooleanLit]= preBodySuc((P.ignoreCase("TRUE") | P.ignoreCase("FALSE")).string).map(BooleanLit(_, _, _))

  val integerBody: P[String] = P.charIn('0' to '9').rep.string.backtrack
  val intLit: P[IntLit]= preBodySuc(integerBody.string).map(IntLit(_, _, _))

  val stringLitBody: P0[String] = (P.charWhere(_ != '\'') | P.string("''")).rep0.string
  val stringLit: P[StringLit] = preBodySuc(P.char('\'') *> stringLitBody <* P.char('\'')).map(StringLit(_, _, _))

  val lit: P[Lit] = booleanLit | intLit | stringLit

  // types
  val lenArg: P[LenArg] = (t("(") ~ intLit ~ t(")")).map(unnest).map(LenArg.apply)
  def lenArg(min: Int, max: Int = Int.MaxValue): P[LenArg] = lenArg.filter(len => min <= len.len.value && len.len.value <= max)
  val baseType: P[BaseType] = (
    /*
      PostgreSQL 12.5: quoted type names are not accepted.
      `CREATE TABLE Foo(a "smallint")` results in `ERROR:  type "smallint" does not exist`
    */
    // https://www.postgresql.org/docs/14/datatype.html
    // https://github.com/pgjdbc/pgjdbc/blob/master/pgjdbc/src/main/java/org/postgresql/jdbc/TypeInfoCache.java
    (t("smallint") | t("int2")).map(_.toIdent).map(smallint.apply) |
    (t("integer")  | t("int4")).map(_.toIdent).map(integer .apply) |
    (t("bigint")   | t("int8")).map(_.toIdent).map(bigint  .apply) |

    ((t("numeric") | t("decimal")).map(_.toIdent) ~ {
      val numericScale: P[NumericScale] = (t(",") ~ intLit).map(NumericScale(_, _))
      val numericArgs: P[NumericArgs] = (t("(") ~ intLit ~ numericScale.? ~ t(")"))
          .map(unnest).map(NumericArgs(_, _, _, _))
      numericArgs.?
    }).map(numeric.apply) |

    t("real").map(_.toIdent).map(real.apply) |
    (t("double") ~ t("precision"))
        .map(t => (t._1.toIdent, t._2.toIdent))
        .map(double_precision.apply) |

    (((t("varchar").map(Seq(_)) | (t("character") ~ t("varying")).backtrack.map(_.toList)).map(_.map(_.toIdent))) ~ lenArg(0).?).map(varchar.apply) |
    (((t("char") | t("character")).map(_.toIdent)) ~ lenArg.?).map(char.apply) |
    t("text").map(_.toIdent).map(text.apply) |

    t("bytea").map(_.toIdent).map(bytea.apply) |

    (t("timestamp") ~ lenArg(0, 6).? ~ (t("without") ~ t("time") ~ t("zone")).map(unnest(_).toList).?).backtrack
        .map(unnest).map((head, precision, tail) => timestamp(head.toIdent +: tail.getOrElse(Nil).map(_.toIdent), precision)) |
    (t("timestamp") ~ lenArg(0, 6).? ~ (t("with") ~ t("time") ~ t("zone")).map(unnest(_).toList))
        .map(unnest).map((head, precision, tail) => timestamptz(head.toIdent +: tail.map(_.toIdent), precision)) |
    t("date").map(_.toIdent).map(date.apply) |
    (t("time") ~ lenArg(0, 6).? ~ (t("without") ~ t("time") ~ t("zone")).map(unnest(_).toList).?).backtrack
        .map(unnest).map((head, precision, tail) => time(head.toIdent +: tail.getOrElse(Nil).map(_.toIdent), precision)) |
    (t("time") ~ lenArg(0, 6).? ~ (t("with") ~ t("time") ~ t("zone")).map(unnest(_).toList))
        .map(unnest).map((head, precision, tail) => timetz(head.toIdent +: tail.map(_.toIdent), precision)) |

    t("boolean").map(_.toIdent).map(boolean.apply) |

    t("jsonb").map(_.toIdent).map(jsonb.apply)
  )
  def arrayDim: P[ArrayDim] = (t("[") ~ intLit.? ~ t("]"))
      .map(unnest).map(ArrayDim.apply)
  def tpe: P[Type] = (baseType ~ arrayDim.rep0).map(Type.apply)

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

  // 4.1.6. Operator Precedence
  val expr: P[Expr] = P.recursive { expr =>
    def unaryOpFoldLeft(ops: Seq[Op], expr: Expr): Expr =
        ops.foldLeft(expr)((expr, op) => UnaryOp(op, expr))
    def binOpFoldLeft(left: Expr, opRights: Seq[(Op, Expr)]): Expr =
        opRights.foldLeft(left)((left, opRight) => BinOp(left, opRight._1, opRight._2))
    def binOpTokenFoldLeft(left: Expr, opRights: Seq[(Token, Expr)]): Expr =
        opRights.foldLeft(left)((left, opRight) => BinOp(left, Op(opRight._1), opRight._2))

    val functionCall: P[FunctionCall] = (ident ~ (t("(") ~ expr.repTokenSep(",") ~ t(")")).map(unnest).map(Args.apply).?)
        .map(FunctionCall.apply)

    val typeCast: P[Expr] =
      // TODO
      (t("(") ~ expr ~ t(")")).map(unnest).map(ParenExpr(_, _, _)) |
      lit |
      functionCall
    val arrayAccess: P[Expr] = (typeCast ~ (t("[") ~ expr ~ t("]")).map(unnest).rep0) // TODO slice subscript
        .map((expr, subscripts) => subscripts.foldLeft(expr)((expr, subscript) => ArrayAccess(expr, subscript._1, subscript._2, subscript._3)))
    val unary: P[Expr] = (op.filter(_.body.isOneOf("+", "-")).backtrack.rep0.with1 ~ arrayAccess)
        .map((signs, expr) => signs.foldLeft(expr)((expr, sign) => UnaryOp(sign, expr)))
    val exp: P[Expr] = (unary ~ (op.filter(_.body.isOneOf("^"          )).backtrack ~ unary).rep0).map(binOpFoldLeft)
    val mul: P[Expr] = (exp   ~ (op.filter(_.body.isOneOf("*", "/", "%")).backtrack ~ exp  ).rep0).map(binOpFoldLeft)
    val add: P[Expr] = (mul   ~ (op.filter(_.body.isOneOf("+", "-"     )).backtrack ~ mul  ).rep0).map(binOpFoldLeft)
    val oth: P[Expr] = (add   ~ (op.filter(!_.body.isOneOf("<", ">", "=", "<=", ">=", "<>", "!=")).backtrack ~ add).rep0).map(binOpFoldLeft)
    val rss: P[Expr] = (oth ~ ( // range, membership, and string match
          (t("NOT").? ~ t("BETWEEN") ~ oth ~ t("AND") ~ oth).backtrack
        ).?).map { case (expr, opt) => opt match
          case Some(((((not, between), left), and), right)) => BetweenOp(expr, not.map(Op(_)), Op(between), left, Op(and), right)
          case None => expr
        }
        // TODO IN
        // TODO LIKE, ILIKE, SIMILAR
    val cmp: P[Expr] = (rss ~ (op.filter(_.body.isOneOf("<", ">", "=", "<=", ">=", "<>", "!=")).backtrack ~ rss).rep0).map(binOpFoldLeft)

    // TODO IS TRUE, IS FALSE, IS NULL, IS DISTINCT FROM
    // TODO ISNULL and NOTNULL (nonstandard syntax)

    val not: P[Expr] = (t("NOT").rep0.with1 ~ cmp)
        .map((nots, expr) => nots.foldLeft(expr)((expr, not) => UnaryOp(Op(not), expr)))
    val and: P[Expr] = (not ~ (t("AND").backtrack ~ not).rep0).map(binOpTokenFoldLeft)
    val or : P[Expr] = (and ~ (t("OR" ).backtrack ~ and).rep0).map(binOpTokenFoldLeft)

    or
  }

  val nullability = (t("NOT").?.with1 ~ t("NULL")).map(Nullability(_, _)).backtrack
  val default = (t("DEFAULT") ~ expr).map(Default(_, _))
  val references = (t("REFERENCES") ~ ident ~ (t("(") ~ ident ~ t(")")).map(unnest).map(ReferencesColumn(_, _, _)).?)
      .map(unnest).map(References(_, _, _))
  val columnConstraint: P[ColumnConstraint] =
      nullability |
      default |
      references

  // TODO validate that there are no more than one constraints of the same type (e.g. NULL NULL)
  val column: P[Column] = (ident ~ tpe ~ columnConstraint.rep0)
      .map(unnest).map(Column(_, _, _)).backtrack

  val primaryKey = (t("PRIMARY") ~ t("KEY") ~ t("(") ~ ident.repTokenSep(",") ~ t(")"))
      .map(unnest).map(PrimaryKey(_, _, _, _, _))
  val unique = (t("UNIQUE") ~ t("(") ~ ident.repTokenSep(",") ~ t(")"))
      .map(unnest).map(Unique(_, _, _, _))
  val foreignKey = (t("FOREIGN") ~ t("KEY") ~ t("(") ~ ident.repTokenSep(",") ~ t(")") ~
      t("REFERENCES") ~ ident ~ (t("(") ~ ident.repTokenSep(",") ~ t(")")).map(unnest).map(ReferencesColumns(_, _, _)).?)
      .map(unnest).map(ForeignKey(_, _, _, _, _, _, _, _))
  val tableConstraint: P[TableConstraint] =
      primaryKey |
      unique |
      foreignKey

  def createTable: P[CreateTable] = {
    val entry = column | tableConstraint
    (t("CREATE") ~ t("TABLE") ~ ident ~ t("(") ~ entry.repTokenSep(",") ~ t(")")).map(unnest).map(CreateTable.apply)
  }
