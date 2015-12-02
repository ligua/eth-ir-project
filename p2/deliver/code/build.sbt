lazy val root = (project in file(".")).
  settings(
    name := "Group 21 query engine",
    mainClass in (Compile, run) := Some("main.scala.Main")
  )