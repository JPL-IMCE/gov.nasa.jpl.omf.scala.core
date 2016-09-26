
PgpKeys.useGpg := true

PgpKeys.useGpgAgent := true

pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

// include *.pom as an artifact
publishMavenStyle := true

// do not include all repositories in the POM
pomAllRepositories := false

// make sure no repositories show up in the POM file
pomIncludeRepository := { _ => false }

additionalProperties := {
  <git.branch>
    {git.gitCurrentBranch.value}
  </git.branch>
    <git.commit>
      {git.gitHeadCommit.value.getOrElse("N/A") + (if (git.gitUncommittedChanges.value) "-SNAPSHOT" else "")}
    </git.commit>
    <git.tags>
      {git.gitCurrentTags.value.map(tag => <git.tag>{tag}</git.tag> )}
    </git.tags>
}

pomPostProcess <<= additionalProperties { (additions) =>
  new xml.transform.RuleTransformer(new xml.transform.RewriteRule {
    override def transform(n: xml.Node): Seq[xml.Node] =
      n match {
        case <properties>{props @ _*}</properties> =>
          <properties>{props}{additions}</properties>
        case _ =>
          n
      }
  })
}

git.baseVersion := Versions.version

git.useGitDescribe := true

versionWithGit
