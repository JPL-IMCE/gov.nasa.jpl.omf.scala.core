import sbt._
import Keys._
import xerial.sbt.Pack._
import com.typesafe.sbt.SbtGit._
import com.banno.license.Plugin.LicenseKeys._

object OMFCore extends Build {
  
  object Versions {
    val scala = "2.11.4"
    val scalaCheck = "1.12.1"
    val scalaTest = "2.2.2"
    val specs2 = "2.4.15"
    val scalaz_stream = "0.6a"
    val scalaz = "7.1.0"
  }
    
  val coreLibs = Project(
      "coreLibs",
      file( "coreLibs" ),      
      settings = Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ packSettings ++ Seq(            
        scalaVersion := Versions.scala,
        packExpandedClasspath := true,
        libraryDependencies ++= Seq(
          "org.scalacheck" %% "scalacheck" % Versions.scalaCheck % "test" withSources() withJavadoc(),
          "org.scalatest" %% "scalatest" % Versions.scalaTest withSources() withJavadoc(),
          "org.specs2" %% "specs2" % Versions.specs2 % "compile" withSources() withJavadoc(),
          "org.scalaz.stream" %% "scalaz-stream" % Versions.scalaz_stream % "compile" withSources() withJavadoc(),
          "org.scalaz" %% "scalaz-core" % Versions.scalaz % "compile" withSources() withJavadoc(),
          "org.scalaz" %% "scalaz-effect" % Versions.scalaz % "compile" withSources() withJavadoc()
        ),
        resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
        ( mappings in pack ) := { extraPackFun.value } )
      )
      
  lazy val core = Project(
      "omf-scala-core",
      file( "." ),
      settings = Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ com.banno.license.Plugin.licenseSettings ++ Seq(            
        scalaVersion := Versions.scala,
        removeExistingHeaderBlock := true,
        scalaSource in Compile := baseDirectory.value / "src",
        scalaSource in Test := baseDirectory.value / "test",
        shellPrompt := { state => Project.extract(state).currentRef.project + " @ " + Project.extract(state).get( GitKeys.gitCurrentBranch ) + "> " }
      )
    ) dependsOn ( coreLibs )
          
  val extraPackFun: Def.Initialize[Task[Seq[( File, String )]]] = Def.task[Seq[( File, String )]] {
    def getFileIfExists( f: File, where: String ): Option[( File, String )] = if ( f.exists() ) Some( ( f, s"${where}/${f.getName()}" ) ) else None

    val ivyHome: File = Classpaths.bootIvyHome( appConfiguration.value ) getOrElse sys.error( "Launcher did not provide the Ivy home directory." )
    
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
    val ld: SettingKey[Seq[ModuleID]] = libraryDependencies
    
    // workaround... I found this API that I managed to call...
    // this overrides the classification of all jars -- i.e., it is as if all library dependencies had been classified as "compile".

    // for now... it's a reasonable approaximation of the goal...
    val managed: Classpath = Classpaths.managedJars( Compile, classpathTypes.value, update.value )
    val result: Seq[( File, String )] = managed flatMap { af: Attributed[File] =>
      af.metadata.entries.toList flatMap { e: AttributeEntry[_] =>
        e.value match {
          case null => Seq()
          case m: ModuleID => Seq() ++
            getFileIfExists( new File( ivyHome, s"cache/${m.organization}/${m.name}/srcs/${m.name}-${m.revision}-sources.jar" ), "lib.srcs" ) ++
            getFileIfExists( new File( ivyHome, s"cache/${m.organization}/${m.name}/docs/${m.name}-${m.revision}-javadoc.jar" ), "lib.javadoc" )
          case _ => Seq()
        }
      }
    }
    result
  }
}