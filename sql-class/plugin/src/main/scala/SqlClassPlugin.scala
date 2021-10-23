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
        case ClassWithSqlTable(typeDef @ untpd.TypeDef(name, typeRhs), args) =>
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

          typeRhs match
            case template @ untpd.Template(constr, parentsOrDerived, self, preBody) =>
              import dotty.tools.dotc.core.StdNames
              constr match
                case untpd.DefDef(StdNames.nme.CONSTRUCTOR, paramss, tpt, defRhs) =>
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

                  val augumentedConstr = untpd.cpy.DefDef(constr)(paramss = augumentedParams)
                  val augumentedTemplate = untpd.cpy.Template(template)(constr = augumentedConstr)
                  val augmentedTypeDef = untpd.cpy.TypeDef(typeDef)(rhs = augumentedTemplate)
                  //println(s"augmentedTypeDef.show = ${augmentedTypeDef.show}")
                  augmentedTypeDef

                case _ => super.transform(tree)
            case _ => super.transform(tree)
        case _ => super.transform(tree)

    ctx.compilationUnit.untpdTree = transformer.transform(ctx.compilationUnit.untpdTree)
  end run

  object ClassWithSqlTable extends CaseClassWithAnnotation("jp.ken1ma.SqlClass.SqlTable")
