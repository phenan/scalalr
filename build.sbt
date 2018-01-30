organization := "com.phenan"

name := "scalalr"

version := "2.0"

scalaVersion := "2.12.4"

mainClass in (Compile, run) := Some("com.phenan.scalalr.Main")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scala-lang" % "scala-reflect" % "2.12.4",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.scopt" %% "scopt" % "3.7.0"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
