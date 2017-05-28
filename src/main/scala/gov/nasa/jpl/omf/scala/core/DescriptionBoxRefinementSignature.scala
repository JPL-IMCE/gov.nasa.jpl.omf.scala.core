package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class DescriptionBoxRefinementSignature[omf <: OMF]
(uuid: UUID,
 descriptionBox: UUID,
 refinedDescriptionBox: omf#DescriptionBox)
  extends ModuleEdgeSignature[omf] {

  override val importedModule: omf#Module = refinedDescriptionBox

}
