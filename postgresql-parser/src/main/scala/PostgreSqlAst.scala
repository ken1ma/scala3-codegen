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
  case class SeqSep[+A](elems: Seq[A], seps: Seq[Token] = Nil):
    require((elems.isEmpty && seps.isEmpty) || (elems.size - 1 == seps.size),
        s"elems.size = ${elems.size} while seps.size = ${seps.size}")
    override def toString = if elems.nonEmpty then (elems.init.zip(seps) :+ elems.last).mkString else ""

  case class Ident(pre: Seq[WhitespaceOrComment], body: String, quoted: Boolean = false, suc: Seq[WhitespaceOrComment] = Nil):
    def this(body: String) = this(Nil, body)
    def code = if quoted then s""""$body"""" else body
    override def toString = pre.mkString + code + suc.mkString

  case class Commands(commands: SeqSep[Option[Command]], suc: Seq[WhitespaceOrComment] = Nil):
    def nonEmptyCommands: Seq[Command] = commands.elems.flatten
  trait Command

  case class CreateTable(create: Token, table: Token, ident: Ident, open: Token, content: SeqSep[CreateTableEntry], close: Token) extends Command:
    def columns: Seq[Column] = content.elems.collect { case column: Column => column }
    def pk: Option[PrimaryKey] = content.elems.collectFirst { case pk: PrimaryKey => pk }
    def pkColumns: Seq[Column] = pk match
      case Some(pk) => columns.filter(column => pk.columns.elems.exists(_.body == column.ident.body))
      case None => Nil
    def nonPkColumns = pk match
      case Some(pk) => columns.filter(column => !pk.columns.elems.exists(_.body == column.ident.body))
      case None => columns
  trait CreateTableEntry

  case class Column(ident: Ident, tpe: SqlType, array: Boolean, constraints: Seq[ColumnConstraint]) extends CreateTableEntry:
    def nullability: Option[Nullability] = constraints.collectFirst { case nullability: Nullability => nullability }
    def notNull: Boolean = nullability.exists(_.not.nonEmpty)
    def default: Option[Default] = constraints.collectFirst { case default: Default => default }

  trait ColumnConstraint
  case class Nullability(not: Option[Token], nul: Token) extends ColumnConstraint:
    override def toString = s"${not.mkString}$nul"
  case class Default(default: Token, expr: Expr) extends ColumnConstraint
  case class References(references: Token, table: Ident, columns: SeqSep[Ident]) extends ColumnConstraint

  trait TableConstraint extends CreateTableEntry
  case class PrimaryKey(primary: Token, key: Token, open: Token, columns: SeqSep[Ident], close: Token) extends TableConstraint
  case class Unique    (unique : Token,             open: Token, columns: SeqSep[Ident], close: Token) extends TableConstraint
  case class ForeignKey(foreign: Token, key: Token, open: Token, columns: SeqSep[Ident], close: Token, references: References) extends TableConstraint

  case class CreateIndex(create: Token, index: Token, ident: Ident, on: Token, table: Ident, open: Token, columns: SeqSep[Ident], close: Token) extends Command

  trait SqlType
  case class BOOLEAN  (ident: Ident) extends SqlType
  case class SMALLINT (ident: Ident) extends SqlType
  case class INTEGER  (ident: Ident) extends SqlType
  case class BIGINT   (ident: Ident) extends SqlType
  case class REAL     (ident: Ident) extends SqlType
  case class DOUBLE_PRECISION(dident: Ident, pident: Ident) extends SqlType
  case class NUMERIC  (ident: Ident, numericArgs: Option[NumericArgs]) extends SqlType
  case class NumericArgs(open: Token, precision: IntegerLit, scale: Option[NumericScale], close: Token)
  case class NumericScale(comma: Token, scale: IntegerLit)
  case class TEXT     (ident: Ident) extends SqlType
  case class CHAR     (ident: Ident, len: LenArg) extends SqlType
  case class LenArg(open: Token, lenArg: IntegerLit, close: Token)
  case class VARCHAR  (ident: Ident, len: LenArg) extends SqlType
  case class BYTEA    (ident: Ident) extends SqlType
  case class DATE     (ident: Ident) extends SqlType
  case class TIME     (ident: Ident) extends SqlType
  case class TIMESTAMP(ident: Ident) extends SqlType
  case class JSONB    (ident: Ident) extends SqlType

  trait Expr
  /** Literal */ trait Lit extends Expr
  case class FunctionCall(ident: Ident, args: Option[CallArgs] = None) extends Lit:
    def isCurrentTimestamp = ident.body.equalsIgnoreCase("CURRENT_TIMESTAMP")
  case class CallArgs(open: Token, args: SeqSep[Expr], close: Token)

  case class BooleanLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit:
    require(body.equalsIgnoreCase("FALSE") | body.equalsIgnoreCase("TRUE"), s"illegal body: $body")
    def booleanValue = body.equalsIgnoreCase("TRUE")
  case class IntegerLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit
  case class StringLit(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil) extends Lit:
    override def toString = s"'$body'"

  case class ArrayLit(pre: Token, open: Token, close: Token, suc: Token) extends Lit
  /** Array dimension */ case class ArrayDim(open: Token, size: Option[IntegerLit], close: Token)

  case class Op(pre: Seq[WhitespaceOrComment], body: String, suc: Seq[WhitespaceOrComment] = Nil)
  case class BinOp(left: Expr, op: Op, right: Expr) extends Expr

  // helper constructors
  object Token:
    def apply(body: String): Token = Token(Nil, body)
    def apply(body: String, suc: Seq[WhitespaceOrComment]): Token = Token(Nil, body, suc)
  object Ident:
    def apply(body: String): Ident = Ident(Nil, body)
    def apply(body: String, quoted: Boolean): Ident = Ident(Nil, body, quoted)
    def apply(body: String, suc: Seq[WhitespaceOrComment]): Ident = Ident(Nil, body, suc = suc)
    def apply(body: String, quoted: Boolean, suc: Seq[WhitespaceOrComment]): Ident = Ident(Nil, body, quoted, suc)

  object BooleanLit:
    def apply(body: String): BooleanLit = BooleanLit(Nil, body)
    def apply(value: Boolean): BooleanLit = this(if value then "TRUE" else "FALSE")
  object IntegerLit:
    def apply(body: String): IntegerLit = IntegerLit(Nil, body)
    def apply(value: Int): IntegerLit = this(value.toString)
  object StringLit:
    def apply(body: String): StringLit = StringLit(Nil, body)

  object Op:
    def apply(body: String): Op = Op(Nil, body)

  object Nullability:
    def apply(value: Boolean): Nullability =
      if value then
        Nullability(Some(Token("NOT")), Token(Seq(Whitespace(" ")), "NULL"))
      else
        Nullability(None, Token("NULL"))

object PostgreSqlAstOps:
  import PostgreSqlAst._

  object SeqSep:
    def apply[A](head: A, tail: Seq[(Token, A)]): SeqSep[A] = new SeqSep(head +: tail.map(_._2), tail.map(_._1))
    def apply[A](head: Option[A], tail: Seq[(Token, A)]): SeqSep[A] = new SeqSep(head.toSeq ++ tail.map(_._2), tail.map(_._1))

  // Ident
    //def append(frag: String) = copy(body = body + frag)

  implicit class SeqColumnOps(columns: Seq[Column]):
    def findOrThrow(identBody: String) = columns.find(_.ident.body == identBody).getOrElse(throw new Exception(s"column not found: $identBody"))
