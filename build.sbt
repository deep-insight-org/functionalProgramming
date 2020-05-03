name := "functionalProgramming"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.2"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint"               // enable handy linter warnings
  //"-Yopt-inline-heuristics" dont example during development
  //"-Xfatal-warnings",     // turn compiler warnings into errors
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
