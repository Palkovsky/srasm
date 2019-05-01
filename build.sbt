name := "srasm"
version := "0.1"
scalaVersion := "2.12.7"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
