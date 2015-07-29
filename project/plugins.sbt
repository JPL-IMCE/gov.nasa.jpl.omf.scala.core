
// TODO: Add the JPL MBEE SBT Maven Repository when it is available...
// resolvers += MavenRepository("JPL MBEE", url("http://github.jpl.nasa.gov/mbee.sbt.repository"))

// TODO: Replace with the JPL MBEE SBT Maven Repository resolver when it is available...
(Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")), Option.apply(System.getProperty("JPL_MBEE_REMOTE_REPOSITORY"))) match {
  case (Some(dir), _) =>
    if (new File(dir) / "settings.xml" exists) {
      val cache = new MavenCache("JPL MBEE", new File(dir))
      Seq(
        publishTo := Some(cache),
        resolvers += cache)
    }
    else
      sys.error(s"The JPL_MBEE_LOCAL_REPOSITORY folder, '$dir', does not have a 'settings.xml' file.")
  case (None, Some(url)) => {
    val repo = new MavenRepository("JPL MBEE", url)
    Seq(
      publishTo := Some(repo),
      resolvers += repo)
  }
  case _ => sys.error("Set either -DJPL_MBEE_LOCAL_REPOSITORY=<dir> or -DJPL_MBEE_REMOTE_REPOSITORY=<url> where <dir> is a local Maven repository directory or <url> is a remote Maven repository URL")
}

addSbtPlugin("gov.nasa.jpl.mbee.sbt" % "sbt-mbee-plugin" % "1800.02-1464dd44241282202546252e8db05d17115a436c")
