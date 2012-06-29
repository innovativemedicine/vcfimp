import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object VcfImpBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "VCFImp",
    version := "0.5.0",
    scalaVersion := "2.9.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"
  )

  lazy val vcfimp = Project("vcfimp", file("vcfimp"))
  
  lazy val vcflatten = Project("vcflatten", file("vcflatten")) settings (assemblySettings: _*) dependsOn (vcfimp)
}

