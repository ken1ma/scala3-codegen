Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
   .settings(
      name := "scala3-codegen",
      scalaVersion := "3.0.2",
      scalacOptions ++= Seq(
         "-Yexplicit-nulls",

         // default from https://scastie.scala-lang.org/
         "-deprecation",
         "-encoding", "UTF-8",
         "-feature",
         "-unchecked",
      ),
   )
