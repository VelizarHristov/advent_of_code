ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"
scalacOptions += "-deprecation"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.8"
// Linear algebra
libraryDependencies += "io.github.pityka" %% "saddle-core" % "3.5.0"
libraryDependencies += "io.github.pityka" %% "saddle-ops-inlined" % "3.5.0"
libraryDependencies += "io.github.pityka" %% "saddle-linalg" % "3.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code_2021"
  )
