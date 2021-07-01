val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "agex",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode"                % "junit-interface" % "0.11"  % "test",
      "org.typelevel"              %% "cats-core"       % "2.6.1",
      "dev.zio"                    %% "zio"             % "1.0.9",
      "com.softwaremill.quicklens" %% "quicklens"       % "1.7.4",
      "com.lihaoyi"                %% "pprint"          % "0.6.6",
      "dev.zio"                    %% "zio-test"        % "1.0.9" % "test",
      "dev.zio"                    %% "zio-test-sbt"    % "1.0.9" % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq("-source:future")
  )
