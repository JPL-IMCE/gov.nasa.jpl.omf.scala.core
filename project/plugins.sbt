lazy val root = (project in file(".")).dependsOn(mbeePlugin)

lazy val mbeePlugin = uri("github://git@github.jpl.nasa.gov:secae/sbt.mbee.plugin.git#64fccaba739ad84aa5e803b1485d84e20487d0f5")
