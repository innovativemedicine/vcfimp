name := "VCFImp"

scalaVersion := "2.9.2"

exportJars := true

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "com.github.scopt" %% "scopt" % "2.1.0"
)


seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)
