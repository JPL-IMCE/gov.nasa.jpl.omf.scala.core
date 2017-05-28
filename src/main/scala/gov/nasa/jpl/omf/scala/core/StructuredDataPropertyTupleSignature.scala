package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class StructuredDataPropertyTupleSignature[omf <: OMF]
(uuid: UUID,
 singletonInstanceStructuredDataPropertyContextUUID: omf#SingletonInstanceStructuredDataPropertyContext,
 structuredataProperty: omf#DataRelationshipToStructure)
