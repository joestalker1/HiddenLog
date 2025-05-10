ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version := "0.0.1-SNAPSHOT"
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.chuusai" %% "shapeless" % "2.3.10",
  "io.circe" %% "circe-generic" % "0.13.0",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
ThisBuild / resolvers ++= Seq(
  ("Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
    .withAllowInsecureProtocol(true),
  Resolver.mavenLocal
)
ThisBuild / organization := "logshow"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:postfixOps",
  "-unchecked"
)
ThisBuild / Test / parallelExecution := false
