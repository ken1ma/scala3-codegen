package jp.ken1ma.SqlClass

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.CaseClass
import dotty.tools.dotc.core.StdNames
import dotty.tools.dotc.plugins.PluginPhase

trait PluginPhaseTpdHelper { this: PluginPhase =>
/*
  // transformTypeDef has been discovered in https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/transform/MegaPhase.scala

  override def transformTypeDef(typeDef: TypeDef)(implicit ctx: Context): TypeDef =
    //println(s"#### typeDef = $typeDef")
    typeDef match
      case ClassWithSqlTable(args) =>
        val sql = args.head match
          case Literal(Constant(value)) => value
          case arg => throw new Exception(s"unexpected arg: $arg")
        println(s"#### sql = $sql")
        typeDef

      case _ => typeDef

  object ClassWithSqlTable extends CaseClassWithAnnotation("jp.ken1ma.SqlClass.SqlTable")
*/
  class CaseClassWithAnnotation(annotationName: String):
    /** @return annotationArgs */
    def unapply(typeDef: TypeDef)(implicit ctx: Context): Option[List[Tree]] =
      if typeDef.mods.is(CaseClass) then
        val annotationArgss = typeDef.mods.annotations.flatMap(_ match
          case Apply(Select(New(name), StdNames.nme.CONSTRUCTOR), args)
              if name.denot.info.classSymbol.fullName.toString == annotationName =>
            Some(args.map(_.asInstanceOf[Tree])) // avoid type error, can we avoid the cast?
          case annot => None
        )

        annotationArgss.size match
          case 1 => Some(annotationArgss.head)
          case 0 => None
          case size => throw new Exception(s"more than one @$annotationName")

      else
        None
}
