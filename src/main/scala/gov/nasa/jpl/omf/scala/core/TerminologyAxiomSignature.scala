package gov.nasa.jpl.omf.scala.core

trait TerminologyAxiomSignature[omf <: OMF]
  extends ModuleEdgeSignature[omf] {

  val importedTerminologyBox: omf#TerminologyBox
  override val importedModule: omf#Module = importedTerminologyBox
}
