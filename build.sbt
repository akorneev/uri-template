name := "uri-template"
organization := "com.github.akorneev"
version := "0.1-SNAPSHOT"

scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.12.10", "2.13.1")

ThisBuild / scalafmtOnCompile := true

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "fastparse" % "2.1.3",
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-opt:_",
  "-Xfuture",
  "-Xlint:_",
//  "-Ypartial-unification",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:_",
  "-Ywarn-extra-implicit"
)
