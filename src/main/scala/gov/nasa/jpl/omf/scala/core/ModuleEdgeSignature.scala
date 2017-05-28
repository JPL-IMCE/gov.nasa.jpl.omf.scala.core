package gov.nasa.jpl.omf.scala.core

trait ModuleEdgeSignature[omf <: OMF] {
  val importedModule: omf#Module
}
