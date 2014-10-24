import sbt._
import Keys._

object Common {
  def settings = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := scala,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    )
  )
  private def scala = "2.11.3"
}