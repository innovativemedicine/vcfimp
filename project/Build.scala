import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object VcfImpBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    organization := "ca.innovativemedicine",
    name := "VCFImp",
    version := "0.5.1",
    scalaVersion := "2.9.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"
  )

  lazy val vcfimp = Project("vcfimp", file("vcfimp"))
  
  lazy val vcflatten = Project("vcflatten", file("vcflatten")) settings (vcflattenSettings: _*) dependsOn (vcfimp)

  def vcflattenSettings = assemblySettings ++ Dist.distSettings

  object Dist {
    lazy val dist = TaskKey[Unit]("dist", "Packages up bin files and JARs in a zip.")
    lazy val distName = SettingKey[String]("dist-name", "Name of the dist zip file without the .zip.")
    lazy val distZip = SettingKey[File]("dist-zip", "Zip file to save dist to.")
    lazy val binFiles = SettingKey[Seq[File]]("dist-bin-files", "The bin files to include in bin/.")

    lazy val distSettings = Seq(
      distName in dist <<= (name, version) { (name, version) => name + "-" + version },
      distZip in dist <<= (target in dist, distName in dist) { (t, n) => t / (n + ".zip") },
      binFiles in dist <<= (sourceDirectory in dist) { src => (src / "main"/  "bin" * "*").get },
      dist <<= (distName in dist, distZip in dist, binFiles in dist, outputPath in assembly) map {
        (name, zip, bins, jar) => distTask(name, zip, bins, Seq(jar))
      } dependsOn (assembly)
    )

    private def distTask(name: String, zip: File, bins: Seq[File], jars: Seq[File]) {
      IO.withTemporaryDirectory { dir =>
        val files = (bins map { f => f -> (name + "/bin/" + f.getName()) }) ++
                    (jars map { f => f -> (name + "/lib/" + f.getName()) })
        IO.zip(files, zip)
      }
    }
  }
}

