
lazy val root = (project in file(".")).
  settings(
    name := "Group 21 web crawler",
    mainClass in Compile := Some("main.scala.Main_Object"),
    mainClass in assembly := Some("main.scala.Main_Object")
  )
