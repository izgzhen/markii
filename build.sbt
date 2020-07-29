name := "markii"

description := "Mark II is a prototype Android UI analyzer to test out fancy ideas"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "com.google.guava" % "guava" % "28.2-jre"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30" % Test
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test" // for running "sbt test"
libraryDependencies += "junit" % "junit" % "4.8.1"
// https://mvnrepository.com/artifact/org.apache.commons/commons-text
libraryDependencies += "org.apache.commons" % "commons-text" % "1.8"

libraryDependencies += "com.github.izgzhen" % "msbase.scala" % "master-SNAPSHOT"

// Released with Soot 4.1.0
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.5"
libraryDependencies += "de.upb.cs.swt" % "heros" % "1.2.0"
libraryDependencies += "de.upb.cs.swt" % "axml" % "2.0.0"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "org.smali" % "dexlib2" % "2.3.4"
libraryDependencies += "ca.mcgill.sable" % "polyglot" % "2006"
libraryDependencies += "ca.mcgill.sable" % "jasmin" % "3.0.2"

libraryDependencies += "org.ow2.asm" % "asm" % "7.1"
libraryDependencies += "org.ow2.asm" % "asm-tree" % "7.1"
libraryDependencies += "org.ow2.asm" % "asm-util" % "7.1"
libraryDependencies += "org.ow2.asm" % "asm-commons" % "7.1"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.4.0"
libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

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

Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/generated/options"
Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/generated/singletons"
Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/generated/jastadd"
Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/generated/sablecc"

test in assembly := {}

mainClass := Some("presto.android.Main")