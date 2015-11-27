// set the name of the project
name := "librjmcmc4s"

version := "1.0"

organization := "fr.ign"

// set the Scala version used for the project
scalaVersion := "2.11.7"

// add a maven-style repository
resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "OpenGeo repo" at "http://download.osgeo.org/webdav/geotools/",
  "Bounless" at "http://repo.boundlessgeo.com/main/"
)

// add a test dependency on ScalaCheck
//libraryDependencies ++= Seq(
//  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
//  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
//)

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

libraryDependencies += "org.geotools" % "gt-shapefile" % "13.1"

libraryDependencies += "org.geotools" % "gt-epsg-wkt" % "13.1"

libraryDependencies += "org.geotools" % "gt-cql" % "13.1"

libraryDependencies += "javax.media" % "jai_core" % "1.1.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

EclipseKeys.withSource := true
//