import Keys._

name := "thought"

version := "0.0.1"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
	"io.spray" %%  "spray-json" % "1.3.1",
	"org.scalatest" %% "scalatest" % "2.2.1" % "test"
)