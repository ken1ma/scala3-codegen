package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.Trees.Literal
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report

import jp.ken1ma.postgresql.parser.{PostgreSqlParser, PostgreSqlAst => sql, CatsParseHelper}

// [Compiler Plugin example](https://docs.scala-lang.org/scala3/reference/changed-features/compiler-plugins.html)
class SqlClassPlugin extends StandardPlugin:
  override val name = "SqlClass"
  override val description = "Augments case class with SQL"
  override def init(options: List[String]): List[PluginPhase] = List(new SqlClassPluginPhase)
  // override val optionsHelp = Some(s"-P:$name:sqlOut=filePath")

class SqlClassPluginPhase extends PluginPhase with PluginPhaseUntpdHelper:
  override val phaseName = "sqlClass"

  override val runsAfter = Set("parser")
  override val runsBefore = Set("typer")

  val postgreSqlParser = PostgreSqlParser()

  override def run(using Context): Unit =
    //println(s"## ctx.compilationUnit = ${ctx.compilationUnit}")

    val transformer = new untpd.UntypedTreeMap():
      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
        case ClassWithSqlTable(typeDef @ untpd.TypeDef(name, rhs), args) =>
          if (!typeDef.mods.is(Flags.CaseClass))
            throw new Exception(s"@SqlTable must annotate a case class")
          //println(s"args = $args")

          val sqlParam = args.head match
            case Literal(constant) => constant.stringValue
          //println(s"sqlParam = $sqlParam")

          val commands = postgreSqlParser.commands.parseAll(sqlParam) match
            case Right(ast) => ast
            case Left(err) => throw new Exception(CatsParseHelper.ErrorPrettyPrint(sqlParam, ctx.compilationUnit.toString).prettyPrint(err)) // TODO
          //println(s"commands = $commands")

          val createTable = commands.nonEmptyCommands.collectFirst { _ match
            case createTable: sql.CreateTable => createTable
          }.getOrElse(throw new Exception(s"no CREATE TABLE")) // TODO
          //println(s"createTable = $createTable")

          rhs match
            case template @ untpd.Template(constr, parentsOrDerived, self, preBody) =>
              import dotty.tools.dotc.core.StdNames
              constr match
                case untpd.DefDef(StdNames.nme.CONSTRUCTOR, paramss, tpt, rhs2) =>
                  val params = paramss.head // case class must have at least one parameter list
                  if (params.nonEmpty)
                    report.error(s"case class with @SqlTable must have empty parameter list", constr)

                  val gen = new SqlClassGen
                  val synthesizedParams = createTable.columns.toList.map { column =>
                    val valDef = gen.paramAccessorOf(column)
                    //println(s"valDef = $valDef")
                    valDef
                  }
                  val augumentedParams = synthesizedParams +: paramss.tail

                  // Since TypeDef.copy cannot be accessed due to TypeDef being private[ast]
                  // TypeDef is constructed and modifiers are set
                  val parents = parentsOrDerived // TODO
                  val derived = Nil
                  val augmentedTypeDef = untpd.TypeDef(name, untpd.Template(untpd.DefDef(StdNames.nme.CONSTRUCTOR, augumentedParams, tpt, rhs2), parents, derived, self, preBody))
                      .withMods(typeDef.mods)
                  //println(s"augmentedTypeDef = $augmentedTypeDef")
                  augmentedTypeDef

                case _ => super.transform(tree)
            case _ => super.transform(tree)
        case _ => super.transform(tree)

    ctx.compilationUnit.untpdTree = transformer.transform(ctx.compilationUnit.untpdTree)
  end run

  object ClassWithSqlTable extends ClassWithAnnotation("jp.ken1ma.SqlClass.SqlTable")

/*
  def show(tree: untpd.Tree)(using Context): String = tree match
    case untpd.Select(qualifier, name) => s"Select(${show(qualifier)}, ${show(name)})"
    case valDef @ untpd.ValDef(name, tpt, _) => s"ValDef(${show(name)}, ${show(tpt)}, ${show(valDef.rhs)})"
    case _ => tree.toString

  def show(name: dotty.tools.dotc.core.Names.Name)(using Context): String =
    if      name.isTermName then s"termName($name)"
    else if name.isTypeName then s"typeName($name)"
    else    s"name($name)"
*/
