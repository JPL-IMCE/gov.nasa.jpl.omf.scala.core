package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class DescriptionBoxExtendsClosedWorldDefinitionsSignature[omf <: OMF]
(uuid: UUID,
 descriptionBox: UUID,
 extendedClosedWorldDefinitions: omf#TerminologyBox)
  extends ModuleEdgeSignature[omf] {

  override val importedModule: omf#Module = extendedClosedWorldDefinitions

}
