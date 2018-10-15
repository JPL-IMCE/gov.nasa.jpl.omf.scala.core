package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver

case class InstanceRelationshipValueRestrictionSignature[omf <: OMF[omf]]
( uuid: resolver.api.taggedTypes.InstanceRelationshipValueRestrictionUUID,
  restrictableRelationship: omf#RestrictableRelationship,
  domain: omf#ConceptualEntitySingletonInstance,
  range: omf#ConceptualEntitySingletonInstance )

