lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "latmap"
)

lazy val dependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

lazy val latmap = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "latmap",
    scalaSource in Compile := baseDirectory.value / "src" / "main" / "scala",
    scalaSource in Test := baseDirectory.value / "src" / "main" / "test",
    libraryDependencies ++= dependencies
  )
