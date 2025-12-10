ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"
scalacOptions += "-deprecation"
scalacOptions += "-Wunused:all"
scalacOptions += "-Wdead-code"
scalacOptions += "-Wnumeric-widen"
scalacOptions += "-Woctal-literal"
scalacOptions += "-Wself-implicit"
scalacOptions += "-Xlint:all"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.8"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
// Linear algebra
libraryDependencies += "io.github.pityka" %% "saddle-core" % "3.5.0"
libraryDependencies += "io.github.pityka" %% "saddle-ops-inlined" % "3.5.0"
libraryDependencies += "io.github.pityka" %% "saddle-linalg" % "3.5.0"

libraryDependencies += "com.github.vagmcs" %% "optimus" % "3.4.5"
libraryDependencies += "com.github.vagmcs" %% "optimus-solver-oj" % "3.4.5"

lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code"
  )
