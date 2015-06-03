lazy val root = (project in file(".")).dependsOn(mbeePlugin)

lazy val mbeePlugin = uri(
  "github://git@github.jpl.nasa.gov:secae/sbt.mbee.plugin.git#" +
    "2b76c0408cdd7c45976b3c36101b9397b58dd562")
