package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ConceptDesignationTerminologySignature[omf <: OMF]
(uuid: UUID,
 graphUUID: UUID,
 designatedConcept: omf#Concept,
 designatedTerminology: omf#TerminologyBox)
  extends TerminologyAxiomSignature[omf] {

  override val importedTerminologyBox: omf#TerminologyBox = designatedTerminology
  override val importedModule: omf#Module = designatedTerminology
}
