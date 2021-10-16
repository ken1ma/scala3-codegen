package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}

import jp.ken1ma.postgresql.parser.{PostgreSqlAst => sql}

class SqlClassGen:
  def paramAccessorOf(column: sql.Column)(using Context): untpd.ValDef =
    val name = termName(column.ident.body)
    val tpt = typeOf(column)
    val default = column.default match
      case Some(default) =>
        default.expr match
          case sql.FunctionCall(ident, args) =>
            ident.body.toLowerCase match
              case "current_timestamp" | "transaction_timestamp" | "now" | "statement_timestamp" | "clock_timestamp" =>
                javaTimeInstantNow
              case _ =>
                // FIXME error
                untpd.EmptyTree
          case _ =>
            // FIXME error
            untpd.EmptyTree
      case None =>
        if column.notNull then
          untpd.EmptyTree
        else
          untpd.Ident(termName("None"))
  
    untpd.ValDef(name, tpt, default)
        .withMods(untpd.Modifiers(Flags.ParamAccessor))

  def typeOf(column: sql.Column)(using Context): untpd.Tree =
    val notNullType = column.tpe match
      case sql.Type(base, arrayDims) =>
        val elemType = typeOf(base)
        arrayDims.foldLeft(elemType) { (elemType, arrayDim) => arrayTypeOf(elemType) }

    // nullable column becomes Option
    if column.notNull then
      notNullType
    else
      untpd.AppliedTypeTree(untpd.Ident(typeName("Option")), List(notNullType))

  def typeOf(base: sql.BaseType)(using Context): untpd.Tree = base match
    case sql.smallint(_)     => untpd.Ident(typeName("Short"))
    case sql.integer(_)      => untpd.Ident(typeName("Int"))
    case sql.bigint(_)       => untpd.Ident(typeName("Long"))
    case sql.numeric(_, ps)  => untpd.Ident(typeName("BigDecimal")) // or BigInt
    case sql.real(_)         => untpd.Ident(typeName("Float"))
    case sql.double(_, _)    => untpd.Ident(typeName("Double"))
    case sql.varchar(_, len) => untpd.Ident(typeName("String"))
    case sql.char(_, len)    => untpd.Ident(typeName("String"))
    case sql.text(_)         => untpd.Ident(typeName("String"))
    case sql.bytea(_)        => arrayTypeOf(untpd.Ident(typeName("Byte")))
    case sql.timestamp(_, precision)   => javaTimeInstantType
    case sql.timestamptz(_, precision) => javaTimeInstantType
    case sql.date(_)                   => javaTimeDate
    case sql.time(_, precision)        => javaTimeTime
    case sql.timetz(_, precision)      => javaTimeTime
    case sql.boolean(ident) => untpd.Ident(typeName("Boolean"))
    case sql.jsonb(ident)   => untpd.Ident(typeName("Json")) // FIXME

  // TODO Context should be synthesized
  def javaTime          (using Context) = untpd.Select(untpd.Ident(termName("java")), termName("time"))
  def javaTimeInstant   (using Context) = untpd.Select(javaTime, termName("Instant"))
  def javaTimeInstantNow(using Context) = untpd.Select(javaTimeInstant, termName("now"))
  def javaTimeDate      (using Context) = untpd.Select(javaTime, typeName("Date"))
  def javaTimeTime      (using Context) = untpd.Select(javaTime, typeName("Time"))

  def javaTimeInstantType(using Context) = untpd.Select(javaTime, typeName("Instant"))

  // TODO Context should be synthesized
  def arrayTypeOf(elemType: untpd.Tree)(using Context): untpd.Tree =
      untpd.AppliedTypeTree(untpd.Ident(typeName("Array")), List(elemType))
