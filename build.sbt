import sbt.Keys._

name := "scala-offheap-collections"
version := "0.1-SNAPSHOT"
scalaVersion := "2.11.8"

lazy val core = project
  .in(file("core"))
  .settings(
    moduleName := "core",
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
    )
  )

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(moduleName := "benchmark")
