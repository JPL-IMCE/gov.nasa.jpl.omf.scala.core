
import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._

updateOptions := updateOptions.value.withCachedResolution(true)

import scala.io.Source
import scala.util.control.Exception._

lazy val core = Project("omf-scala-core", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings(Some("gov.nasa.jpl.omf.scala.core")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.core",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,

    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies ++= Seq (
      "gov.nasa.jpl.imce" %% "imce.third_party.other_scala_libraries"
        % Versions_other_scala_libraries.version artifacts
        Artifact("imce.third_party.other_scala_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
    ),

    extractArchives := {},

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg")
  )

def dynamicScriptsResourceSettings(dynamicScriptsProjectName: Option[String] = None): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts/<dynamicScriptsProjectName>'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := {
      val projectName = dynamicScriptsProjectName.getOrElse(baseDirectory.value.getName)
      require(
        QUALIFIED_NAME.pattern.matcher(projectName).matches,
        s"The project name, '$projectName` is not a valid Java qualified name")
      Some(projectName)
    },

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal <++= (
      baseDirectory,
      packageBin in Compile,
      packageSrc in Compile,
      packageDoc in Compile,
      packageBin in Test,
      packageSrc in Test,
      packageDoc in Test) map {
      (base, bin, src, doc, binT, srcT, docT) =>
        val dir = base / "svn" / "org.omg.oti"
        (dir ** "*.dynamicScripts").pair(relativeTo(dir)) ++
          (dir ** "*.md").pair(relativeTo(dir)) ++
          addIfExists(bin, "lib/" + bin.name) ++
          addIfExists(binT, "lib/" + binT.name) ++
          addIfExists(src, "lib.sources/" + src.name) ++
          addIfExists(srcT, "lib.sources/" + srcT.name) ++
          addIfExists(doc, "lib.javadoc/" + doc.name) ++
          addIfExists(docT, "lib.javadoc/" + docT.name)
    },

    artifacts <+= (name in Universal) { n => Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) },
    packagedArtifacts <+= (packageBin in Universal, name in Universal) map { (p, n) =>
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}