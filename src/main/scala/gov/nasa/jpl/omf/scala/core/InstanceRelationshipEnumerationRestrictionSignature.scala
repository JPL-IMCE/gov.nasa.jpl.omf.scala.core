package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver
import scala.collection.immutable.Vector

case class InstanceRelationshipEnumerationRestrictionSignature[omf <: OMF[omf]]
( uuid: resolver.api.taggedTypes.InstanceRelationshipEnumerationRestrictionUUID,
  restrictableRelationship: omf#RestrictableRelationship,
  domain: omf#ConceptualEntitySingletonInstance,
  references: Vector[omf#ConceptualEntitySingletonInstance] )

  