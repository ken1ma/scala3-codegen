ThisBuild / scalaVersion := "3.1.0-RC2"
ThisBuild / scalacOptions := Seq(
  // default from https://scastie.scala-lang.org/
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",

  // java.lang.String.toLowerCase results in String | Null, and can't be assigned to String
  // java.time.Instant.now results in Instant | Null, and can't be assigned to Instant
  //"-Yexplicit-nulls",

  //"-explain", // Explain errors in more detail
)

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "scala3-codegen",
    scalacOptions ++= Seq(
      s"-Xplugin:${(plugin / Compile / packageBin).value.getAbsolutePath}",
      s"-P:SqlClass:lastModified=${(plugin / Compile / packageBin).value.lastModified}", // recompile when the plugin changes
    ),
    libraryDependencies ++= Seq(
      // pureconfig seem to be not yet published for Scala 3
      //("com.github.pureconfig" %% "pureconfig-magnolia" % "0.16.0").cross(CrossVersion.for3Use2_13),

      "org.typelevel" %% "cats-effect" % "3.2.9",
      "org.typelevel" %% "cats-effect-cps" % "0.3.0",
      "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1",
      //"org.postgresql" % "postgresql" % "42.2.23",
    ),
  )
  .dependsOn(annotation, plugin)

lazy val plugin = (project in file("sql-class/plugin"))
  .settings(
    name := "sql-class-plugin",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    ),
  )
  .dependsOn(`postgresql-parser`)

lazy val annotation = (project in file("sql-class/annotation"))
  .settings(
    name := "sql-class-annotation",
  )

lazy val `postgresql-parser` = (project in file("postgresql-parser"))
  .settings(
    name := "postgresql-parser",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.4",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    )
  )
