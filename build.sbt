name := "markii"

description := "Mark II is a prototype Android UI analyzer to test out fancy ideas"

version := "0.1"

// scala version needs to align with SBT version
// https://stackoverflow.com/questions/49000201/whats-the-relationship-of-the-versions-of-scala-when-i-use-sbt-to-build-a-scala
scalaVersion := "2.12.4"

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "com.google.guava" % "guava" % "28.2-jre"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test" // for running "sbt test"
libraryDependencies += "junit" % "junit" % "4.8.1"
// https://mvnrepository.com/artifact/org.apache.commons/commons-text
libraryDependencies += "org.apache.commons" % "commons-text" % "1.8"

libraryDependencies += "com.github.rohanpadhye" % "vasco" % "5598bcf6dc"
libraryDependencies += "com.github.izgzhen" % "msbase.scala" % "master-SNAPSHOT"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.4.0"

val circeVersion = "0.12.0"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-yaml"
).map(_ % circeVersion)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

test in assembly := {}

mainClass := Some("presto.android.Main")