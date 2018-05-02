name := "Sentiment Analysis Project"

version := "1.1"

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-unchecked", "-deprecation")

initialCommands in console := 
  """
  import Main._
  """

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.3.0"

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "2.3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
