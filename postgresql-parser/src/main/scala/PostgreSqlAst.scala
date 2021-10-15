package jp.ken1ma.postgresql
package parser

object PostgreSqlAst:
  trait WhitespaceOrComment:
    def body: String
    override def toString = body
  case class Whitespace(body: String) extends WhitespaceOrComment

  trait Comment extends WhitespaceOrComment
  case class LineComment(body: String, newLineOrEof: Option[String]) extends Comment:
    override def toString = s"--$body${newLineOrEof.mkString}"
  case class BlockComment(body: String) extends Comment:
    override def toString = s"/*$body*/"

  /**
   * Keyword or identifier.
   * To reproduce the parsed SQL string, the whitespaces/comments around the token are kept with the body.
   * @param pre the whitespaces/comments preceding the body
   * @param suc the whitespaces/comments succeeding the body up to a newline or EOF
   */
  case class Token(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil)

  /** Seq with token separators */
  case class SeqTokenSep[+A](elems: Seq[A], seps: Seq[Token]):
    require((elems.isEmpty && seps.isEmpty) || (elems.size - 1 == seps.size),
        s"elems.size = ${elems.size} while seps.size = ${seps.size}")
    override def toString = (elems.init.zip(seps) ++ elems.lastOption).mkString

  case class Ident(pre: Seq[WhitespaceOrComment], body: String, quoted: Boolean = false, suc: Seq[WhitespaceOrComment] = Nil):
    def this(body: String) = this(Nil, body)
    def code = if quoted then s""""$body"""" else body
    override def toString = pre.mkString + code + suc.mkString

  case class Commands(commands: SeqTokenSep[Option[Command]], suc: Seq[WhitespaceOrComment] = Nil):
    def nonEmptyCommands: Seq[Command] = commands.elems.flatten
  trait Command

  case class CreateTable(create: Token, table: Token, ident: Ident, open: Token, entries: SeqTokenSep[CreateTableEntry], close: Token) extends Command:
    def columns: Seq[Column] = entries.elems.collect { case column: Column => column }
    def pk: Option[PrimaryKey] = entries.elems.collectFirst { case pk: PrimaryKey => pk }
    def pkColumns: Seq[Column] = pk match
      case Some(pk) => columns.filter(column => pk.columns.elems.exists(_.body == column.ident.body))
      case None => Nil
    def nonPkColumns = pk match
      case Some(pk) => columns.filter(column => !pk.columns.elems.exists(_.body == column.ident.body))
      case None => columns
  trait CreateTableEntry

  case class Column(ident: Ident, tpe: Type, constraints: Seq[ColumnConstraint] = Nil) extends CreateTableEntry:
    def nullability: Option[Nullability] = constraints.collectFirst { case nullability: Nullability => nullability }
    def notNull: Boolean = nullability.exists(_.notNull)
    def default: Option[Default] = constraints.collectFirst { case default: Default => default }

  trait ColumnConstraint
  case class Nullability(not: Option[Token], nul: Token) extends ColumnConstraint:
    def notNull: Boolean = not.nonEmpty
    override def toString = s"${not.mkString}$nul"
  case class Default(default: Token, expr: Expr) extends ColumnConstraint
  case class References(references: Token, table: Ident, dstColumn: Option[ReferencesColumn] = None) extends ColumnConstraint
  case class ReferencesColumn(open: Token, column: Ident, close: Token)

  trait TableConstraint extends CreateTableEntry
  case class PrimaryKey(primary: Token, key: Token, open: Token, columns: SeqTokenSep[Ident], close: Token) extends TableConstraint
  case class Unique    (unique : Token,             open: Token, columns: SeqTokenSep[Ident], close: Token) extends TableConstraint
  case class ForeignKey(foreign: Token, key: Token, open: Token, columns: SeqTokenSep[Ident], close: Token,
      references: Token, table: Ident, dstColumns: Option[ReferencesColumns]) extends TableConstraint {
    require(dstColumns.forall(_.columns.elems.size == columns.elems.size))
  }
  case class ReferencesColumns(open: Token, columns: SeqTokenSep[Ident], close: Token)

  case class CreateIndex(create: Token, index: Token, ident: Ident, on: Token, table: Ident, open: Token, columns: SeqTokenSep[Ident], close: Token) extends Command

  trait BaseType
  /** INT2 */ case class smallint (ident: Ident) extends BaseType
  /** INT4 */ case class integer  (ident: Ident) extends BaseType
  /** INT8 */ case class bigint   (ident: Ident) extends BaseType
  case class numeric  (ident: Ident, numericArgs: Option[NumericArgs] = None) extends BaseType
  case class NumericArgs(open: Token, precision: IntLit, scale: Option[NumericScale] = None, close: Token)
  case class NumericScale(comma: Token, scale: IntLit)
  case class real     (ident: Ident) extends BaseType
  case class double_precision(ident1: Ident, ident2: Ident) extends BaseType
  // SMALLSERIAL
  // SERIAL
  // BIGSERIAL
  // MONEY
  case class varchar  (idents: Seq[Ident], len: Option[LenArg] = None) extends BaseType
  case class LenArg(open: Token, len: IntLit, close: Token)
  case class char     (ident: Ident, len: Option[LenArg]) extends BaseType
  case class text     (ident: Ident) extends BaseType
  case class bytea    (ident: Ident) extends BaseType
  /** timestamp [without time zone] */ case class timestamp(idents: Seq[Ident], precision: Option[LenArg] = None) extends BaseType:
    override def toString = s"${idents.head}$precision${idents.tail}"
  /** timestamp  with    time zone  */ case class timestamptz(idents: Seq[Ident], precision: Option[LenArg] = None) extends BaseType:
    override def toString = s"${idents.head}$precision${idents.tail}"
  case class date     (ident: Ident) extends BaseType
  /** time [without time zone] */ case class time(idents: Seq[Ident], precision: Option[LenArg] = None) extends BaseType
  /** time  with    time zone  */ case class timetz(idents: Seq[Ident], precision: Option[LenArg] = None) extends BaseType
  // INTERVAL
  case class boolean  (ident: Ident) extends BaseType
  // ENUM / Composite Types / Domain Types
  // POINT, LINE, LSEG, BOX, PATH, POLYGON, CIRCLE
  // CIDR, INET, MACADDR, MACADDR8
  // BIT, VBIT
  // TSVECTOR, TSQUERY
  // UUID
  // XML
  // JSON
  case class jsonb    (ident: Ident) extends BaseType
  /** Array dimension */ case class ArrayDim(open: Token, size: Option[IntLit], close: Token)
  // INT4RANGE, INT8RANGE, NUMRANGE, TSRANGE, TSTZRANGE, DATERANGE
  // OID
  // PG_LSN
  // Pseudo-Types

  case class Type(base: BaseType, arrayDims: Seq[ArrayDim] = Nil)

  trait Expr
  case class ParenExpr(open: Token, expr: Expr, close: Token) extends Expr

  /** Literal */ trait Lit extends Expr
  case class FunctionCall(ident: Ident, args: Option[Args] = None) extends Lit:
    def isCurrentTimestamp = ident.body.equalsIgnoreCase("CURRENT_TIMESTAMP")
  case class Args(open: Token, args: SeqTokenSep[Expr], close: Token)

  case class BooleanLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit:
    require(body.equalsIgnoreCase("FALSE") | body.equalsIgnoreCase("TRUE"), s"illegal body: $body")
    def booleanValue = body.equalsIgnoreCase("TRUE")
  case class IntLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit:
    val value = body.toInt
  case class StringLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit:
    override def toString = s"'$body'"

  case class ArrayLit(pre: Token, open: Token, close: Token, suc: Token) extends Lit

  case class ArrayAccess(expr: Expr, open: Token, subscript: Expr, close: Token) extends Expr
  case class Op(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil)
  case class UnaryOp(op: Op, expr: Expr) extends Expr
  case class BinOp(left: Expr, op: Op, right: Expr) extends Expr
  case class BetweenOp(expr: Expr, not: Option[Op], between: Op, left: Expr, and: Op, right: Expr) extends Expr
    //def lower = if left <= right then left else right
    //def upper = if left <= right then right else left

  // helper constructors
  object Token:
    def apply(body: String, suc: Seq[WhitespaceOrComment]): Token = this(Nil, body, suc)
    def apply(body: String): Token = this(Nil, body)
    def apply(pre: Whitespace, body: String): Token = this(Seq(pre), body)

  object SeqTokenSep:
    def apply[A](head: A, tail: Seq[(Token, A)] = Nil): SeqTokenSep[A] = SeqTokenSep(head +: tail.map(_._2), tail.map(_._1))
    def apply[A](head: Option[A], tail: Seq[(Token, A)]): SeqTokenSep[A] = SeqTokenSep(head.toSeq ++ tail.map(_._2), tail.map(_._1))
    def apply[A](elems: Seq[A], sep: Token): SeqTokenSep[A] = SeqTokenSep(elems, Seq.fill(elems.size - 1)(sep))

  object Ident:
    def apply(body: String, suc: Seq[WhitespaceOrComment]): Ident = this(Nil, body, suc = suc)
    def apply(body: String): Ident = this(Nil, body)
    def apply(body: String, quoted: Boolean, suc: Seq[WhitespaceOrComment]): Ident = this(Nil, body, quoted, suc)
    def apply(body: String, quoted: Boolean): Ident = this(Nil, body, quoted)
    def apply(pre: Whitespace, body: String, quoted: Boolean): Ident = this(Seq(pre), body, quoted)
    def apply(pre: Whitespace, body: String): Ident = this(Seq(pre), body)

  object timestamp:
    def apply(ident: Ident): timestamp = this(Seq(ident))

  object BooleanLit:
    def apply(body: String, suc: Seq[WhitespaceOrComment]): BooleanLit = this(Nil, body, suc)
    def apply(body: String): BooleanLit = this(Nil, body)
    def apply(pre: Seq[WhitespaceOrComment], value: Boolean, suc: Seq[WhitespaceOrComment]): BooleanLit = this(pre, if value then "TRUE" else "FALSE", suc)
    def apply(pre: Seq[WhitespaceOrComment], value: Boolean): BooleanLit = this(pre, value, Nil)
    def apply(value: Boolean): BooleanLit = this(Nil, value)

  object IntLit:
    def apply(body: String, suc: Seq[WhitespaceOrComment]): IntLit = this(Nil, body, suc)
    def apply(body: String): IntLit = this(Nil, body)
    def apply(pre: Seq[WhitespaceOrComment], value: Int, suc: Seq[WhitespaceOrComment]): IntLit = this(pre, value.toString, suc)
    def apply(pre: Seq[WhitespaceOrComment], value: Int): IntLit = this(pre, value, Nil)
    def apply(value: Int): IntLit = this(Nil, value)

  object StringLit:
    def apply(body: String): StringLit = this(Nil, body)
    def apply(pre: Whitespace, body: String): StringLit = this(Seq(pre), body)

  object Op:
    def apply(body: String): Op = this(Nil, body)
    def apply(token: Token): Op = this(token.pre, token.body, token.suc)

  object BetweenOp:
    def apply(expr: Expr, between: Op, left: Expr, and: Op, right: Expr): BetweenOp = this(expr, None, between, left, and, right)

  object Nullability:
    def apply(value: Boolean): Nullability =
      if value then
        Nullability(Some(Token("NOT")), Token(Seq(Whitespace(" ")), "NULL"))
      else
        Nullability(None, Token("NULL"))

  object Column:
    def apply(ident: Ident, baseType: BaseType, constraints: ColumnConstraint*): Column = this(ident, Type(baseType), constraints.toSeq)

object PostgreSqlAstOps:
  import PostgreSqlAst._

  implicit class SeqColumnOps(columns: Seq[Column]):
    def findOrThrow(identBody: String) = columns.find(_.ident.body == identBody).getOrElse(throw new Exception(s"column not found: $identBody"))
