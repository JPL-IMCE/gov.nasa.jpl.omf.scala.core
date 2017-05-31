package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class TerminologyExtensionSignature[omf <: OMF]
(uuid: UUID,
 extendedTerminology: omf#TerminologyBox)
  extends TerminologyAxiomSignature[omf] {

  override val importedTerminologyBox: omf#TerminologyBox = extendedTerminology
  override val importedModule: omf#Module = extendedTerminology
}
