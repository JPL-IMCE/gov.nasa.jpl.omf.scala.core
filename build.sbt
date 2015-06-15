import java.io.File

import sbt.Keys._
import sbt._

import gov.nasa.jpl.mbee.sbt._

lazy val core = Project("omf-scala-core", file(".")).
  settings(GitVersioning.buildSettings). // in principle, unnecessary; in practice: doesn't work without this
  enablePlugins(MBEEGitPlugin).
  settings(
    MBEEKeys.mbeeLicenseYearOrRange := "2014-2015",
    MBEEKeys.mbeeOrganizationInfo := MBEEPlugin.MBEEOrganizations.imce,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",

    // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
    resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

    libraryDependencies ++= Seq (
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion("jpl-mbee-common-scala-libraries_core", MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision),
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion("jpl-mbee-common-scala-libraries_other", MBEEKeys.mbeeReleaseVersionPrefix.value, Versions.jpl_mbee_common_scala_libraries_revision)
    ),

    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := Some("dynamicScripts/gov.nasa.jpl.omf.scala.core"),
    mappings in Universal <++= (baseDirectory,
      packageBin in Compile, packageSrc in Compile, packageDoc in Compile,
      packageBin in Test, packageSrc in Test, packageDoc in Test) map {
      (dir, bin, src, doc, binT, srcT, docT) =>
        (dir ** "*.dynamicScripts").pair(relativeTo(dir)) ++
          (dir ** "*.md").pair(relativeTo(dir)) ++
          (dir / "models" ** "*.mdzip").pair(relativeTo(dir)) ++
          com.typesafe.sbt.packager.MappingsHelper.directory(dir / "resources") ++
          Seq(
            (bin, "lib/" + bin.name),
            (binT, "lib/" + binT.name),
            (src, "lib.sources/" + src.name),
            (srcT, "lib.sources/" + srcT.name),
            (doc, "lib.javadoc/" + doc.name),
            (docT, "lib.javadoc/" + docT.name)
          )
    },
    artifacts <+= (name in Universal) { n => Artifact(n, "jar", "jar", Some("resource"), Seq(), None, Map()) },
    packagedArtifacts <+= (packageBin in Universal, name in Universal) map { (p,n) =>
      Artifact(n, "jar", "jar", Some("resource"), Seq(), None, Map()) -> p
    },

    aether.AetherKeys.aetherArtifact <<=
      (aether.AetherKeys.aetherCoordinates,
        aether.AetherKeys.aetherPackageMain,
        makePom in Compile,
        packagedArtifacts in Compile) map {
        (coords: aether.MavenCoordinates, mainArtifact: File, pom: File, artifacts: Map[Artifact, File]) =>
          aether.AetherPlugin.createArtifact(artifacts, pom, coords, mainArtifact)
      }
  )