import sbt._
import sbt.Keys._

object SugarkovetesBuild extends Build {

  lazy val proj = Project(
    id = "sugerkovetes",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "sugarkovetes",
      organization := "pityu",
      version := "1.0",
      scalaVersion := "2.9.2",
      // add other settings here
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "1.8" % "test"
        )
    )
  )
}
