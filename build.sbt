scalaVersion := "3.7.4"
scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wsafe-init",
)
semanticdbEnabled := true

name := "advent-2025"
version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)
