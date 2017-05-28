package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ReifiedRelationshipInstanceDomainSignature[omf <: OMF]
( uuid: UUID,
  reifiedRelationshipInstance: omf#ReifiedRelationshipInstance,
  domain: omf#ConceptualEntitySingletonInstance )
