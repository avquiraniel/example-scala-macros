name := "testMacros"

version := "1.0"

lazy val testMacros = project in file(".") settings(commonSettings ++ mainSettings: _*) aggregate (main, macros)

lazy val macrosSpecificSettings = Seq(libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _))
lazy val mainSettings = Seq(
  mainClass in Compile := Some("Playground")
)
lazy val main = project in file("./main") settings (commonSettings: _*) dependsOn macros

lazy val macros = project in file("./macros") settings (commonSettings ++ macrosSpecificSettings: _*)

lazy val commonSettings = Defaults.coreDefaultSettings ++ Seq(
  scalaVersion := "2.11.5",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature"
  )
)

    