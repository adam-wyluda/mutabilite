import sbt._

object OffheapCollectionsBuild extends Build {
  lazy val root = Project(id = "root",
    base = file("."))

  lazy val benchmark = Project(id = "benchmark",
    base = file("benchmark")) dependsOn(root)
}
