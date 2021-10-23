package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.CaseClass
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
              name.denot.info.classSymbol.fullName.toString returns the fully qualified name after typer
              TODO: somehow resolve the fullName
              tpt.denot = val <none>
              tpt.denot.info = NoType
              ctx.typer.findRef(name, defn.AnyType, EmptyFlags, EmptyFlags, tree.srcPos) returns NoType for both Ident(name) and Select(qualifier, name)
                same with Types.WildcardType
              ctx.typer.symbolOfTree(name) throws IllegalArgumentException: SqlTable does not have a symbol
              the followings print out [error] Not found: type SqlTable
                ctx.typer.typedAnnotation(annotTree) 
                ctx.typer.typed(tpt)
                ctx.typer.typedUnadapted(tpt)
            */
            // compare the last name as a last resort (doesn't work when renamed while importing)
            val nameLast = annotationName.lastIndexOf('.') match
              case -1 => annotationName
              case lastDot => annotationName.drop(lastDot + 1)
            val nameMatched = tpt match
              case Ident(name)             => name.toString == nameLast
              case Select(qualifier, name) => name.toString == nameLast
            Option.when(nameMatched)(args.map(_.asInstanceOf[Tree])) // avoid type error, can we avoid the cast?

          case annot => None
        )

        annotationArgss.size match
          case 1 => Some((typeDef, annotationArgss.head))
          case 0 => None
          case size => throw new Exception(s"more than one @$annotationName")

      else
        None
}
