name := "binaro"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.scalaz" %% "scalaz-effect" % "7.1.3",
  "org.scalaz" %% "scalaz-typelevel" % "7.1.3",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.3" % "test"
)
