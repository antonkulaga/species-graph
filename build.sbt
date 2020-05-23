import sbt.Keys._
import com.typesafe.sbt.packager.docker.{Cmd, DockerChmodType, ExecCmd}
import sbt._

name := "species-graph"

organization := "group.aging-research"

version := "0.1.0"

isSnapshot := false

crossScalaVersions := Seq("2.13.2","2.12.11")

scalaVersion := "2.13.2"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.bintrayRepo("comp-bio-aging", "main")

bintrayRepository := "main"

bintrayOrganization := Some("comp-bio-aging")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

libraryDependencies += "org.eclipse.rdf4j" % "rdf4j-sail" % "3.1.2"

libraryDependencies += "org.eclipse.rdf4j" % "rdf4j-query" % "3.1.2"

libraryDependencies += "org.eclipse.rdf4j" % "rdf4j-repository-api" % "3.1.2"

libraryDependencies += "org.eclipse.rdf4j" % "rdf4j-repository-manager" % "3.1.2"

libraryDependencies += "org.eclipse.rdf4j" % "rdf4j-sparqlbuilder" % "3.1.2"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.4"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"

libraryDependencies += "com.monovore" %% "decline" % "1.0.0"

libraryDependencies += "com.monovore" %% "decline-enumeratum" % "1.0.0"

libraryDependencies += "org.wvlet.airframe" %% "airframe-log" % "20.5.1"

libraryDependencies += "org.scalatest" %% "scalatest-wordspec" % "3.2.0-M4" % Test

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

dockerBaseImage := "oracle/graalvm-ce:20.0.0-java8"

daemonUserUid in Docker := None

daemonUser in Docker := "root"

dockerExposedVolumes := Seq("/data")

dockerUpdateLatest := true

dockerChmodType := DockerChmodType.UserGroupWriteExecute

maintainer in Docker := "Anton Kulaga <antonkulaga@gmail.com>"

maintainer := "Anton Kulaga <antonkulaga@gmail.com>"

dockerRepository := Some("quay.io/comp-bio-aging")

testOptions in Test += Tests.Argument("-oD")

dockerCommands ++= Seq(
  Cmd("WORKDIR", "/data"),
  Cmd("ENV", "JAVA_OPTS", "-Xmx6g")
)

enablePlugins(JavaAppPackaging, DockerPlugin)