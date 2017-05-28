package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ReifiedRelationshipInstanceRangeSignature[omf <: OMF]
( uuid: UUID,
  reifiedRelationshipInstance: omf#ReifiedRelationshipInstance,
  range: omf#ConceptualEntitySingletonInstance )
