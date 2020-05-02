import sbt.Keys._

import sbt._

name := "species-graph"

organization := "group.aging-research"

version := "0.0.6"

isSnapshot := false

crossScalaVersions := Seq("2.12.11","2.13.2")

scalaVersion := "2.12.11"

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