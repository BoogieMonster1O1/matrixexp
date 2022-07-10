ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "matrixexp",
    idePackagePrefix := Some("io.github.boogiemonster1o1.matrixexp")
  )
