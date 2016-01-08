
import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._

scmInfo := Some(ScmInfo(
  url("https://github.jpl.nasa.gov/imce/imce.sbt.plugin"),
  "git@github.jpl.nasa.gov:imce/imce.sbt.plugin.git"))

developers := List(
  Developer(
    id="rouquett",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://gateway.jpl.nasa.gov/personal/rouquett/default.aspx")))

lazy val core = Project("omf-scala-core", file("."))
  .enablePlugins(IMCEGitPlugin)
  .settings(IMCEPlugin.packageReleaseProcessSettings: _*)
  //.settings(GitVersioning.buildSettings) // in principle, unnecessary; in practice: doesn't work without this
  .settings(IMCEPlugin.dynamicScriptsProjectResourceSettings(Some("gov.nasa.jpl.omf.scala.core")))
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    useGpg := true,
    useGpgAgent := true,
//    git.useGitDescribe := true,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    classDirectory in Compile := baseDirectory.value / "bin",
    scalaSource in Test := baseDirectory.value / "test",
    classDirectory in Test := baseDirectory.value / "bin.tests",

    // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
    // resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

    libraryDependencies ++= Seq (
      "gov.nasa.jpl.imce.thirdParty" %% "all-scala-libraries" % Versions.jpl_mbee_common_scala_libraries artifacts 
      Artifact("all-scala-libraries", "zip", "zip"),
      "gov.nasa.jpl.imce.thirdParty" %% "other-scala-libraries" % Versions.jpl_mbee_common_scala_libraries artifacts 
      Artifact("other-scala-libraries", "zip", "zip")
    )
  )
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
//  .settings(versionWithGit: _*)
