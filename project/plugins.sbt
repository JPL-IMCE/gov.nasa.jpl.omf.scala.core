import java.nio.file.FileSystems

// TODO: Add the JPL MBEE SBT Maven Repository when it is available...
// resolvers += MavenRepository("JPL MBEE", url("http://github.jpl.nasa.gov/mbee.sbt.repository"))

// https://bintray.com/banno/oss/sbt-license-plugin/view
resolvers +=
  Resolver.url("sbt-license-plugin-releases", url("http://dl.bintray.com/banno/oss"))(Resolver.ivyStylePatterns)

// TODO: Replace with the JPL MBEE SBT Maven Repository resolver when it is available...
( Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")),
  Option.apply(System.getProperty("JPL_MBEE_REMOTE_REPOSITORY"))) match {
  case (Some(loc), _) =>
    val dir = FileSystems.getDefault.getPath(loc)
    val settings = dir.resolve("settings.xml").toFile
    if (settings exists) {
      val cache = new MavenCache("JPL MBEE", dir.toFile)
      Seq(
        publishTo := Some(cache),
        resolvers += cache)
    }
    else
      sys.error(s"The JPL_MBEE_LOCAL_REPOSITORY folder, '$loc', does not have a 'settings.xml' file.")
  case (None, Some(url)) => {
    val repo = new MavenRepository("JPL MBEE", url)
    Seq(
      publishTo := Some(repo),
      resolvers += repo)
  }
  case _ => sys.error("Set either -DJPL_MBEE_LOCAL_REPOSITORY=<dir> or -DJPL_MBEE_REMOTE_REPOSITORY=<url> where <dir> is a local Maven repository directory or <url> is a remote Maven repository URL")
}

addSbtPlugin("gov.nasa.jpl.mbee.sbt" % "sbt-mbee-plugin" % "1800.02-ceee63c3e1772cbbefeddc2376c50d0ca0297ebf")