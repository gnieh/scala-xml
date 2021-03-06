val scala211 = "2.11.11"
val scala212 = "2.12.3"

lazy val commonSettings = Seq(
  organization := "org.gnieh",
  scalaVersion := scala212,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.3" % Test,
    "com.github.pathikrit" %% "better-files" % "2.17.1" % Test),
  version := "0.1.0-SNAPSHOT",
  description := "Scala XML library revisited",
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://github.com/gnieh/scala-xml")),
  crossScalaVersions := Seq(scala211, scala212),
  parallelExecution := false,
  fork in test := true,
  scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits"),
  autoAPIMappings := true,
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Ypartial-unification")) ++ publishSettings

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  // The Nexus repo we're publishing to.
  publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
  ),
  pomIncludeRepository := { x => false },
  pomExtra := (
    <scm>
      <url>https://github.com/gnieh/scala-xml</url>
      <connection>scm:git:git://github.com/gnieh/scala-xml.git</connection>
      <developerConnection>scm:git:git@github.com:gnieh/scala-xml.git</developerConnection>
      <tag>HEAD</tag>
    </scm>
    <developers>
      <developer>
        <id>satabin</id>
        <name>Lucas Satabin</name>
        <email>lucas.satabin@gnieh.org</email>
      </developer>
    </developers>
    <ciManagement>
      <system>travis</system>
      <url>https://travis-ci.org/#!/gnieh/scala-xml</url>
    </ciManagement>
    <issueManagement>
      <system>github</system>
      <url>https://github.com/gnieh/scala-xml/issues</url>
    </issueManagement>
  ))

lazy val xml = project.in(file("."))
  .enablePlugins(ScoverageSbtPlugin, ScalaUnidocPlugin)
  .settings(commonSettings)
  .settings(
    name := "xml",
    packagedArtifacts := Map())
  .aggregate(core, std)

lazy val core = project.in(file("core"))
  .enablePlugins(ScoverageSbtPlugin)
  .settings(commonSettings)
  .settings(
    name := "xml-core")

lazy val std = project.in(file("std"))
  .enablePlugins(ScoverageSbtPlugin)
  .settings(commonSettings)
  .settings(
    name := "xml-std")
  .dependsOn(core)
