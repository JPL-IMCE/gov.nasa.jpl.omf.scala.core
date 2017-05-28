package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class BundledTerminologySignature[omf <: OMF]
(uuid: UUID,
 bundle: UUID,
 bundledTerminology: omf#TerminologyBox)
  extends TerminologyAxiomSignature[omf] {

  override val importedTerminologyBox: omf#TerminologyBox = bundledTerminology

}