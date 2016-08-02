import sbt.Keys._

name := "scala-offheap-collections"

lazy val defaults = Defaults.coreDefaultSettings ++ Seq(
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val api = project
  .in(file("api"))
  .settings(
    defaults ++ Seq(
      moduleName := "api"
    )
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(api)
  .settings(
    defaults ++ Seq(
      moduleName := "tests",
      libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.11" % "2.2.4",
        "org.scalacheck" %% "scalacheck" % "1.12.2"
      )
    )
  )

lazy val specializedCore = project
  .in(file("specialized-core"))
  .dependsOn(api, tests % "test")
  .settings(
    defaults ++ Seq(
      moduleName := "specialized-core"
    )
  )

lazy val specializedMacros = project
  .in(file("specialized-macros"))
  .dependsOn(specializedCore)
  .settings(
    defaults ++ Seq(
      moduleName := "specialized-macros",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % "2.11.8"
      )
    )
  )

lazy val specializedOps = project
  .in(file("specialized-ops"))
  .dependsOn(api, specializedMacros, tests % "test")
  .settings(
    defaults ++ Seq(
      moduleName := "specialized-ops"
    )
)

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(specializedOps)
  .enablePlugins(JmhPlugin)
  .settings(
    defaults ++ Seq(
      moduleName := "benchmark"
    )
  )
