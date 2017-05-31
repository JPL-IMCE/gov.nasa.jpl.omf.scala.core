package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class TerminologyNestingSignature[omf <: OMF]
(uuid: UUID,
 nestingContext: omf#Concept,
 nestingTerminology: omf#TerminologyBox)
  extends TerminologyAxiomSignature[omf] {

  override val importedTerminologyBox: omf#TerminologyBox = nestingTerminology
  override val importedModule: omf#Module = nestingTerminology

}
