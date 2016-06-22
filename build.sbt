import sbt.Keys._

name := "scala-offheap-collections"
version := "0.1-SNAPSHOT"
val scalaVer = "2.11.8"

lazy val api = project
  .in(file("api"))
  .settings(
    moduleName := "api",
    libraryDependencies ++= Seq(
      "sh.den" % "scala-offheap_2.11" % "0.1"
    ),
    scalaVersion := scalaVer
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(api)
  .settings(
    moduleName := "tests",
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4",
      "org.scalacheck" %% "scalacheck" % "1.12.2"
    ),
    scalaVersion := scalaVer
  )

lazy val naive = project
  .in(file("naive"))
  .dependsOn(api, tests % "test")
  .settings(
    moduleName := "naive",
    scalaVersion := scalaVer
  )

lazy val specialized = project
  .in(file("specialized"))
  .dependsOn(api, tests % "test")
  .settings(
    moduleName := "specialized",
    scalaVersion := scalaVer
  )

lazy val generic = project
  .in(file("generic"))
  .dependsOn(api, tests % "test")
  .settings(
    moduleName := "generic",
    scalaVersion := scalaVer
  )

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(generic, specialized)
  .enablePlugins(JmhPlugin)
  .settings(
    moduleName := "benchmark",
    scalaVersion := scalaVer
  )
