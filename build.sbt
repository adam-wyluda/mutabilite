import sbt.Keys._

name := "scala-offheap-collections"

lazy val defaults = Defaults.coreDefaultSettings ++ Seq(
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val testDependencies = Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.2"
)

lazy val core = project
  .in(file("core"))
  .settings(
    defaults ++ Seq(
      moduleName := "core",
      libraryDependencies ++= testDependencies
    )
  )

lazy val macros = project
  .in(file("macros"))
  .dependsOn(core)
  .settings(
    defaults ++ Seq(
      moduleName := "macros",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % "2.11.8"
      )
    )
  )

lazy val ops = project
  .in(file("ops"))
  .dependsOn(macros)
  .settings(
    defaults ++ Seq(
      moduleName := "ops",
      libraryDependencies ++= testDependencies
    )
)

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(ops)
  .enablePlugins(JmhPlugin)
  .settings(
    defaults ++ Seq(
      moduleName := "benchmark",
      resolvers += Resolver.sonatypeRepo("releases"),
      libraryDependencies ++= Seq(
        "org.spire-math" %% "debox" % "0.7.3"
      )
    )
  )
