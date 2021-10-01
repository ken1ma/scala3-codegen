package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}

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

  override def run(using Context): Unit =
    println(s"## ctx.compilationUnit = ${ctx.compilationUnit}")
    val debug = ctx.compilationUnit.toString.endsWith("/SqlClassDoobie.scala")

    val transformer = new untpd.UntypedTreeMap():
      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match
        case ClassWithSqlTable(typeDef @ untpd.TypeDef(name, rhs), args) =>
          if (!typeDef.mods.is(Flags.CaseClass))
            throw new Exception(s"@SqlTable must be attached to a case class")
          println(s"args = $args")

          rhs match
            case template @ untpd.Template(constr, parentsOrDerived, self, preBody) =>
              import dotty.tools.dotc.core.StdNames
              constr match
                case untpd.DefDef(StdNames.nme.CONSTRUCTOR, paramss, tpt, rhs2) =>
                  paramss match
                    case List(Nil) =>
                      // Since TypeDef.copy cannot be accessed since TypeDef is private[ast]
                      // TypeDef is constructed and modifiers are set
                      val valDef = untpd.ValDef(termName("id"), untpd.Ident(typeName("Int")), untpd.EmptyTree)
                          .withMods(untpd.Modifiers(Flags.ParamAccessor))
                      val paramss = List(List(valDef))
                      val parents = parentsOrDerived // TODO
                      val derived = Nil
                      val augmented = untpd.TypeDef(name, untpd.Template(untpd.DefDef(StdNames.nme.CONSTRUCTOR, paramss, tpt, rhs2), parents, derived, self, preBody))
                          .withMods(typeDef.mods)
                      augmented

                    case _ => super.transform(tree)
                case _ => super.transform(tree)
            case _ => super.transform(tree)
        case _ => super.transform(tree)

    ctx.compilationUnit.untpdTree = transformer.transform(ctx.compilationUnit.untpdTree)

  object ClassWithSqlTable extends ClassWithAnnotation("jp.ken1ma.SqlClass.SqlTable")
