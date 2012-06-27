name := "VCFImp"

scalaVersion := "2.9.2"

exportJars := true

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "2.1.0"
)


seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)
