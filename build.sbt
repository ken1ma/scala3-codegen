Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
   .settings(
      name := "scala3-codegen",
      scalaVersion := "3.0.2",
      scalacOptions ++= Seq(
         // java.lang.String.toLowerCase results in String | Null, and can't be assigned to String
         // java.time.Instant.now results in Instant | Null, and can't be assigned to Instant
         //"-Yexplicit-nulls",

         //"-explain", // Explain errors in more detail

         // default from https://scastie.scala-lang.org/
         "-deprecation",
         "-encoding", "UTF-8",
         "-feature",
         "-unchecked",
      ),
      libraryDependencies ++= Seq(
         // pureconfig seem to be not yet published for Scala 3
         //("com.github.pureconfig" %% "pureconfig-magnolia" % "0.16.0").cross(CrossVersion.for3Use2_13),

         "org.typelevel" %% "cats-effect" % "3.2.8",
         "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1",
         //"org.postgresql" % "postgresql" % "42.2.23",
      ),
   )
