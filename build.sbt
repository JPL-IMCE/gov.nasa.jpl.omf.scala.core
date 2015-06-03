
def jpl_mbee_imce_artifact(versionPrefix: String, artifactId: String): sbt.ModuleID = {
  val version: String = versionPrefix+"-"+Versions.jpl_mbee_common_scala_libraries_revision
  "gov.nasa.jpl.mbee.imce" %% artifactId % version
}


lazy val core = Project("omf-scala-core",
  file (".")).
  settings(GitVersioning.buildSettings). // in principle, unecessary; in practice: doesn't work without this
  enablePlugins(MBEEGitPlugin).
  settings(
    mbeeLicenseYearOrRange := "2015",
    mbeeOrganizationInfo := MBEEPlugin.Organizations.imce,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    libraryDependencies ++= Seq (
      "gov.nasa.jpl.mbee.imce" %% "jpl-mbee-common-scala-libraries_core" % "1800-02-220be6985e6117a94f58dd1b897e8f143c65e64d" artifacts(Artifact("jpl-mbee-common-scala-libraries_core", "zip", "zip")),
      "gov.nasa.jpl.mbee.imce" %% "jpl-mbee-common-scala-libraries_other" % "1800-02-220be6985e6117a94f58dd1b897e8f143c65e64d" artifacts(Artifact("jpl-mbee-common-scala-libraries_other", "zip", "zip"))
      //jpl_mbee_imce_artifact("jpl-mbee-common-scala-libraries_other", mbeeReleaseVersionPrefix.value)
    )
)