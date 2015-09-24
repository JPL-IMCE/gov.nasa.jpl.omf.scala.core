
import sbt.Keys._
import sbt._

import gov.nasa.jpl.mbee.sbt._


lazy val core = Project("omf-scala-core", file("."))
  .settings(GitVersioning.buildSettings) // in principle, unnecessary; in practice: doesn't work without this
  .enablePlugins(MBEEGitPlugin)
  .settings(MBEEPlugin.mbeeDynamicScriptsProjectResourceSettings(Some("gov.nasa.jpl.omf.scala.core")))
  .settings(
    MBEEKeys.mbeeLicenseYearOrRange := "2014-2015",
    MBEEKeys.mbeeOrganizationInfo := MBEEPlugin.MBEEOrganizations.imce,
    MBEEKeys.targetJDK := MBEEKeys.jdk18.value,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    classDirectory in Compile := baseDirectory.value / "bin",
    scalaSource in Test := baseDirectory.value / "test",
    classDirectory in Test := baseDirectory.value / "bin.tests",

    // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
    resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

    libraryDependencies ++= Seq (
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion("jpl-mbee-common-scala-libraries_core", MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision),
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion("jpl-mbee-common-scala-libraries_other", MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision)
    )
  )
  .settings(MBEEPlugin.mbeeStrictScalacFatalWarningsSettings)