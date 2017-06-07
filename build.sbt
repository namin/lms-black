name := "lms-black"

organization := "org.scala-lang.lms"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers += Resolver.sonatypeRepo("snapshots")

//libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT"

libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "0.9.0"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test"

scalacOptions += "-Yvirtualize"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

parallelExecution in Test := false

initialCommands in console := "import scala.lms.black._; import repl._; import eval._; def assertResult(expected: Any)(actual: Any) = assert (expected==actual)"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

logBuffered := false
