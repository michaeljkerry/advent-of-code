name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "2.0.1-RC1",
  "dev.optics" %% "monocle-core"  % "3.0.0",
  "dev.optics" %% "monocle-macro" % "3.0.0", // only for Scala 2.13
)

test2
test2