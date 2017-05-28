package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class UnreifiedRelationshipInstanceTupleSignature[omf <: OMF]
( uuid: UUID,
  unreifiedRelationship: omf#UnreifiedRelationship,
  domain: omf#ConceptualEntitySingletonInstance,
  range: omf#ConceptualEntitySingletonInstance )
