( Option.apply(System.getProperty("JPL_LOCAL_RESOLVE_REPOSITORY")),
  Option.apply(System.getProperty("JPL_REMOTE_RESOLVE_REPOSITORY")) ) match {
  case (Some(dir), _) =>
    if ((new File(dir) / "settings.xml").exists) {
      val cache = new MavenCache("JPL Resolve", new File(dir))
      Seq(resolvers += cache)
    }
    else
      sys.error(s"The JPL_LOCAL_RESOLVE_REPOSITORY folder, '$dir', does not have a 'settings.xml' file.")
  case (None, Some(url)) => {
    val repo = new MavenRepository("JPL Resolve", url)
    Seq(resolvers += repo)
  }
  case _ => sys.error("Set either -DJPL_LOCAL_RESOLVE_REPOSITORY=<dir> or"+
                      "-DJPL_REMOTE_RESOLVE_REPOSITORY=<url> where"+
                      "<dir> is a local Maven repository directory or"+
                      "<url> is a remote Maven repository URL")
}

// https://bintray.com/banno/oss/sbt-license-plugin/view
resolvers += Resolver.url(
  "sbt-license-plugin-releases",
  url("http://dl.bintray.com/banno/oss"))(Resolver.ivyStylePatterns)

addSbtPlugin("gov.nasa.jpl.imce" % "imce-sbt-plugin" % "2.5.2")
