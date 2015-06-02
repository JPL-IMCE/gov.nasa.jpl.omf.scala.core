
lazy val core = Project("omf-scala-core",
  file (".")).
  enablePlugins(MBEEGitPlugin).
  settings(
    mbeeLicenseYearOrRange := "2015",
    mbeeReleaseVersionPrefix := "1800-02",
    mbeeOrganizationInfo := MBEEPlugin.Organizations.imce,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies ++= Seq (
      "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_core" % Versions.jpl_mbee_core,
      "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_other" % Versions.jpl_mbee_other
    )
)