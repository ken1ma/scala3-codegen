package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags, Flags.CaseClass
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.plugins.PluginPhase

trait PluginPhaseUntpdHelper { this: PluginPhase =>
  class CaseClassWithAnnotation(annotationName: String):
    /** @return annotationArgs */
    def unapply(typeDef: TypeDef)(implicit ctx: Context): Option[(TypeDef, List[Tree])] =
      if typeDef.mods.is(CaseClass) then
        val annotationArgss = typeDef.mods.annotations.flatMap(_ match
          case Apply(Select(New(tpt), StdNames.nme.CONSTRUCTOR), args) =>
            /*
              we want a symbol that identifies the annotation definition that namer computes
              however we want to run before typer since we are generating class members

              but it seems namer and typer remains as a single phase
              https://github.com/lampepfl/dotty/pull/13762 

              and there seems to be no typer plugins mentioned in
              https://github.com/lampepfl/dotty/pull/3438

              I don't think we can resolve imports or type aliases
            */

            //println(s"tpt = $tpt") // prints "Ident(SqlTable)"
            //println(s"tpt.denot = ${tpt.denot}") // prints "val <none>"
            //println(s"tpt.denot.info = ${tpt.denot.info}") // prints "NoType"
            //denot.info.classSymbol.fullName.toString returns the fully qualified name after typer
            /*
            tpt match
              case Ident(name) =>
                //println(s"  name = $name") // prints "SqlTable"
                //println(s"  findRef = ${ctx.typer.findRef(name, ctx.definitions.AnyType, Flags.EmptyFlags, Flags.EmptyFlags, tpt.srcPos)}") // prints "NoType"
                //ctx.typer.symbolOfTree(tpt) throws IllegalArgumentException: SqlTable does not have a symbol
                //ctx.typer.typedAnnotation(annotTree) prints "Not found: type SqlTable"
                //ctx.typer.typed(tpt)                 prints "Not found: type SqlTable"
                //ctx.typer.typedUnadapted(tpt)        prints "Not found: type SqlTable"
              case _ =>
            */

            // compare the last name as a last resort (doesn't work when renamed while importing)
            val nameLast = annotationName.lastIndexOf('.') match
              case -1 => annotationName
              case lastDot => annotationName.drop(lastDot + 1)
            val nameMatched = tpt match
              case Ident(name)             => name.toString == nameLast
              case Select(qualifier, name) => name.toString == nameLast
            Option.when(nameMatched)(args)

          case _ => None
        )

        annotationArgss.size match
          case 1 => Some((typeDef, annotationArgss.head))
          case 0 => None
          case size => throw new Exception(s"more than one @$annotationName")

      else
        None
}
