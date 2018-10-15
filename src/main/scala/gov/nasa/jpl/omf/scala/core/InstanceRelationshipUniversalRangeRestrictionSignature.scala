package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver

case class InstanceRelationshipUniversalRangeRestrictionSignature[omf <: OMF[omf]]
( uuid: resolver.api.taggedTypes.InstanceRelationshipUniversalRangeRestrictionUUID,
  restrictableRelationship: omf#RestrictableRelationship,
  domain: omf#ConceptualEntitySingletonInstance,
  range: omf#Entity )

