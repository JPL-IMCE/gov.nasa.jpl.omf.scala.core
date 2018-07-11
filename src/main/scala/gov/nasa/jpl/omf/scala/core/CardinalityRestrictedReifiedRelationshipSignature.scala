package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables.{CardinalityRestrictionKind, taggedTypes}

import scala.Option

case class CardinalityRestrictedReifiedRelationshipSignature[omf <: OMF[omf]]
(uuid: resolver.api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
 name: taggedTypes.LocalName,
 iri: omf#IRI,
 restrictionKind: CardinalityRestrictionKind,
 restrictedRelationship: omf#RestrictableRelationship,
 restrictedRange: Option[omf#Entity],
 restrictedCardinality: taggedTypes.PositiveIntegerLiteral)