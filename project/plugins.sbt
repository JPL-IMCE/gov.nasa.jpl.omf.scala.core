
resolvers += Resolver.url("sbt-license-plugin-releases", url("http://dl.bintray.com/banno/oss"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.banno" % "sbt-license-plugin" % "0.1.4")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.6.5")

// The Typesafe repository
resolvers += "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.8")

// https://github.com/sbt/sbt-assembly
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")
