ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"
scalacOptions += "-deprecation"
lazy val root = (project in file("."))
  .settings(
    name := "advent_of_code_2021"
  )
