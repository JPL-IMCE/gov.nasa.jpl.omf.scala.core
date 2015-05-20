import java.io.File

import com.banno.license.Plugin.LicenseKeys._
import com.typesafe.sbt.SbtGit._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbt.Keys._
import sbt._
import xerial.sbt.Pack._

/**
 * sbt \
 * -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)>
 */
object OMFCore extends Build {

  lazy val jplSettings = Seq(
    scalaVersion := Versions.scala,
    organization := "gov.nasa.jpl.mbee.omf",
    organizationName := "JPL, Caltech",
    organizationHomepage := Some(url("https://mbse.jpl.nasa.gov")),
    publishMavenStyle := false,
    publishTo := {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Some(Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns))
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    }
  )

  lazy val commonSettings =
    Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ graphSettings

  lazy val aggregateDependenciesPublishSettings = Seq(
    // disable publishing the main jar produced by `package`
    publishArtifact in(Compile, packageBin) := false,

    // disable publishing the main API jar
    publishArtifact in(Compile, packageDoc) := false,

    // disable publishing the main sources jar
    publishArtifact in(Compile, packageSrc) := false
  )

  lazy val dependenciesPackSettings = packSettings ++ Seq(
    packExpandedClasspath := true,
    packLibJars := Seq.empty,
    packExcludeArtifactTypes := Seq("src", "doc"),
    (mappings in pack) := {
      extraPackFun.value
    }
  ) ++ publishPackZipArchive

  val coreLibs = Project(
    "coreLibs",
    file("coreLibs")).
    settings(versionWithGit: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(aggregateDependenciesPublishSettings: _*).
    settings(dependenciesPackSettings: _*).
    settings(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % Versions.scala % "compile" withSources() withJavadoc(),
        "org.scala-lang" % "scala-library" % Versions.scala % "compile" withSources() withJavadoc(),
        "org.scala-lang" % "scala-compiler" % Versions.scala % "compile" withSources() withJavadoc(),
        "org.scala-lang.modules" %% "scala-xml" % Versions.scala_xml % "compile" withSources() withJavadoc(),
        "org.scala-lang.modules" %% "scala-parser-combinators" % Versions.scala_parser_combinators % "compile" withSources() withJavadoc(),
        "org.scalacheck" %% "scalacheck" % Versions.scalaCheck % "test" withSources() withJavadoc(),
        "org.scalatest" %% "scalatest" % Versions.scalaTest withSources() withJavadoc(),
        "org.specs2" %% "specs2-core" % Versions.specs2 % "compile" withSources() withJavadoc(),
        "org.scalaz.stream" %% "scalaz-stream" % Versions.scalaz_stream % "compile" withSources() withJavadoc(),
        "org.scalaz" %% "scalaz-core" % Versions.scalaz % "compile" withSources() withJavadoc(),
        "org.scalaz" %% "scalaz-effect" % Versions.scalaz % "compile" withSources() withJavadoc()
      )
    )

  val playLibs = Project(
    "playLibs",
    file("playLibs")).
    settings(versionWithGit: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(aggregateDependenciesPublishSettings: _*).
    settings(dependenciesPackSettings: _*).
    settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play" % Versions.play % "compile" withSources(),
        "com.typesafe.play" %% "play-iteratees" % Versions.play % "compile" withSources(),
        "com.typesafe.play" %% "play-json" % Versions.play % "compile" withSources(),
        "com.typesafe.play" %% "play-functional" % Versions.play % "compile" withSources()
      ),
      libraryDependencies ~= {
        _ map {
          case m if m.organization == "com.typesafe.play" =>
            m.
              exclude("commons-codec", "commons-codec").
              exclude("commons-logging", "commons-logging").
              exclude("com.typesafe", "config").
              exclude("com.typesafe.play", "sbt-link").
              exclude("org.slf4j", "slf4j-api").
              exclude("org.slf4j", "slf4j-nop")
          case m => m
        }
      }
    )


  lazy val sourcePublishSettings = Seq(
    // disable publishing the main jar produced by `package`
    publishArtifact in(Compile, packageBin) := true,

    // disable publishing the main API jar
    publishArtifact in(Compile, packageDoc) := true,

    // disable publishing the main sources jar
    publishArtifact in(Compile, packageSrc) := true
  )

  def mappingFromProject(mappings: ((Seq[TaskKey[File]], Seq[Configuration]), String)*)(currentProject: ProjectRef, structure: BuildStructure): Task[Seq[(File, String)]] = {
    (mappings flatMap { case ((targetTasks: Seq[TaskKey[File]], configs: Seq[Configuration]), where: String) =>
      targetTasks flatMap { t: TaskKey[File] =>
        configs map { c =>
          Def.task {
            val file = ((t in c) in currentProject).value
            (file, where + "/" + file.getName)
          } evaluate structure.data
        }
      }
    }).join
  }

  lazy val sourcePackSettings = packSettings ++ Seq(
    packExpandedClasspath := false,
    packLibJars := Seq.empty,
    packUpdateReports := Seq.empty,
    mappings in pack <<= (thisProjectRef, buildStructure) flatMap mappingFromProject(
      (Seq(packageBin), Seq(Compile, Test)) -> "lib",
      (Seq(packageSrc), Seq(Compile, Test)) -> "lib.srcs",
      (Seq(packageDoc), Seq(Compile, Test)) -> "lib.javadoc"
    )
  ) ++ publishPackZipArchive

  lazy val core = Project(
    "omf-scala-core",
    file(".")).
    settings(versionWithGit: _*).
    settings(showCurrentGitBranch: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(sourcePublishSettings: _*).
    settings(com.banno.license.Plugin.licenseSettings: _*).
    settings(sourcePackSettings: _*).
    settings(
      removeExistingHeaderBlock := true,
      scalaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "test"
    ).
    dependsOn(coreLibs, playLibs) //enablePlugins( play.PlayScala )

  val extraPackFun: Def.Initialize[Task[Seq[(File, String)]]] = Def.task[Seq[(File, String)]] {
    def getFileIfExists(f: File, where: String): Option[(File, String)] = if (f.exists()) Some((f, s"$where/${f.getName}")) else None

    val ivyHome: File = Classpaths.bootIvyHome(appConfiguration.value) getOrElse sys.error("Launcher did not provide the Ivy home directory.")

    // this is a workaround; how should it be done properly in sbt?

    // goal: process the list of library dependencies of the project.
    // that is, we should be able to tell the classification of each library dependency module as shown in sbt:
    //
    // > show libraryDependencies
    // [info] List(
    //    org.scala-lang:scala-library:2.11.2, 
    //    org.scala-lang:scala-library:2.11.2:provided, 
    //    org.scala-lang:scala-compiler:2.11.2:provided, 
    //    org.scala-lang:scala-reflect:2.11.2:provided, 
    //    com.typesafe:config:1.2.1:compile, 
    //    org.scalacheck:scalacheck:1.11.5:compile, 
    //    org.scalatest:scalatest:2.2.1:compile, 
    //    org.specs2:specs2:2.4:compile, 
    //    org.parboiled:parboiled:2.0.0:compile)

    // but... libraryDependencies is a SettingKey (see ld below)
    // I haven't figured out how to get the sequence of modules from it.
    //val ld: SettingKey[Seq[ModuleID]] = libraryDependencies

    // workaround... I found this API that I managed to call...
    // this overrides the classification of all jars -- i.e., it is as if all library dependencies had been classified as "compile".

    // for now... it's a reasonable approaximation of the goal...
    val managed: Classpath = Classpaths.managedJars(Compile, classpathTypes.value, update.value)
    val result: Seq[(File, String)] = managed flatMap { af: Attributed[File] =>
      af.metadata.entries.toList flatMap { e: AttributeEntry[_] =>
        e.value match {
          case null => Seq()
          case m: ModuleID => Seq() ++
            getFileIfExists(new File(ivyHome, s"cache/${m.organization}/${m.name}/srcs/${m.name}-${m.revision}-sources.jar"), "lib.srcs") ++
            getFileIfExists(new File(ivyHome, s"cache/${m.organization}/${m.name}/docs/${m.name}-${m.revision}-javadoc.jar"), "lib.javadoc")
          case _ => Seq()
        }
      }
    }
    result
  }
}