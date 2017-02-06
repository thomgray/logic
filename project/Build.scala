import com.waioeka.sbt.CucumberPlugin
import sbt._
import sbt.dsl._
import sbt.{Build => SbtBuild}
import sbt.Keys._
import sbtassembly.Plugin._
import sbtassembly.Plugin.AssemblyKeys._

object Build extends SbtBuild {
  val Organization = "gray"
  val Name = "logic"
  val Version = Option(System.getenv("BUILD_VERSION")) getOrElse "DEV"
  val ScalaVersion = "2.11.8"

  val dependencies = Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "info.cukes" %% "cucumber-scala" % "1.2.4" % "test",
    "info.cukes" % "cucumber-junit" % "1.2.4",

    "org.slf4j" % "slf4j-api" % "1.7.5",
    "org.slf4j" % "slf4j-simple" % "1.7.5",
    "org.clapper" %% "grizzled-slf4j" % "1.3.0",

    "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
    "org.scala-lang.modules" % "scala-jline" % "2.12.1"
  )

  lazy val project = Project(
    Name,
    file("."),
    settings =
      Defaults.coreDefaultSettings ++
        assemblySettings ++
        Seq(
          CucumberPlugin.glue := "cucumber/steps/",
          CucumberPlugin.features := List("cucumber"),
          unmanagedResourceDirectories in Compile <+= baseDirectory(_ / "test/fixtures")
        ) ++
        Seq(scalacOptions ++= Seq("-feature", "-target:jvm-1.7", "-language:postfixOps")) ++
        Seq(
          organization := Organization,
          name := Name,
          version := Version,
          scalaVersion := ScalaVersion,
          resolvers += Classpaths.typesafeReleases,
          jarName in assembly := s"$Name.jar",
          mainClass in assembly := Some("Main"),
          mergeStrategy in assembly := {
            case PathList("mime.types") => MergeStrategy.first
            case x =>
            val oldStrategy = (mergeStrategy in assembly).value
            oldStrategy(x)
          },
          libraryDependencies ++= dependencies
        )
  ).enablePlugins(CucumberPlugin)

}
