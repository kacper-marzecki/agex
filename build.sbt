val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.typelevel" %% "cats-core" % "2.6.1",
      "dev.zio" %% "zio" % "1.0.9",
      "dev.zio" %% "zio-test" % "1.0.9" % "test",
      "dev.zio" %% "zio-test-sbt" % "1.0.9" % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
