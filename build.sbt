val scala3Version = "2.13.8"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val root = project
  .in(file("."))
  .settings(
    name := "agex",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode"                % "junit-interface"  % "0.11"  % "test",
      "org.typelevel"              %% "cats-core"        % "2.6.1",
      "dev.zio"                    %% "zio"              % "1.0.9",
      "dev.zio"                    %% "zio-interop-cats" % "2.5.1.0",
      "com.softwaremill.quicklens" %% "quicklens"        % "1.7.4",
      "com.lihaoyi"                %% "pprint"           % "0.6.6",
      "org.typelevel"              %% "cats-parse"       % "0.3.4",
      "dev.zio"                    %% "zio-test"         % "1.0.9" % "test",
      "dev.zio"                    %% "zio-test-sbt"     % "1.0.9" % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
