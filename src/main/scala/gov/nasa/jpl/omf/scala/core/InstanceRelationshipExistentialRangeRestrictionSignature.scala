package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver

case class InstanceRelationshipExistentialRangeRestrictionSignature[omf <: OMF[omf]]
( uuid: resolver.api.taggedTypes.InstanceRelationshipExistentialRangeRestrictionUUID,
  restrictableRelationship: omf#RestrictableRelationship,
  domain: omf#ConceptualEntitySingletonInstance,
  range: omf#Entity )

