name := "srasm"
version := "0.1"
scalaVersion := "2.11.11"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "com.github.scopt" % "scopt_2.11" % "4.0.0-RC2"

