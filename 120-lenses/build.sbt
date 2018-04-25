import sbt._

name := "Lenses"

version := "0.0"


// resolvers += Resolver.sonatypeRepo("releases")
// 
// resolvers += Resolver.sonatypeRepo("snapshots")
// 
// scalaVersion   := "2.11"   // or "2.10.6"

scalaVersion := "2.12.5"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" 
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

/*val libraryVersion = "1.2.0-M1" // or "1.3.0-SNAPSHOT"*/

/*libraryDependencies ++= Seq(*/
  /*"com.github.julien-truffaut"  %%  "monocle-core"    % libraryVersion,*/
  /*"com.github.julien-truffaut"  %%  "monocle-generic" % libraryVersion,*/
  /*"com.github.julien-truffaut"  %%  "monocle-macro"   % libraryVersion,*/
  /*"com.github.julien-truffaut"  %%  "monocle-state"   % libraryVersion,*/
  /*"com.github.julien-truffaut"  %%  "monocle-law"     % libraryVersion % "test"*/
/*)*/

val monocleVersion = "1.5.0" 

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test"
)

// addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions += "-deprecation"

scalacOptions += "-feature"

/*libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"*/

/*libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"*/

//libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.3.1"

// libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "compile"

val scalazVersion = "7.2.21"

libraryDependencies ++= Seq ( 
  "org.scalaz" %% "scalaz-core"               % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test" )

