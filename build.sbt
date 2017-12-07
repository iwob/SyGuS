
// NOTE: jars in lib/ directory are automatically added to the classpath.

lazy val root = (project in file(".")).
  settings(
    name := "SyGuS Scala Parser",
    version := "1.0",
    scalaVersion := "2.11.11",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "junit" % "junit" % "4.12" % Test,
        "com.novocode" % "junit-interface" % "0.11" % Test)
  )
