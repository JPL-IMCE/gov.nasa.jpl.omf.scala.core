package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables.{CardinalityRestrictionKind, taggedTypes}

import scala.Option

case class CardinalityRestrictedAspectSignature[omf <: OMF[omf]]
(uuid: resolver.api.taggedTypes.CardinalityRestrictedAspectUUID,
 name: taggedTypes.LocalName,
 iri: omf#IRI,
 restrictionKind: CardinalityRestrictionKind,
 restrictedRelationship: omf#RestrictableRelationship,
 restrictedRange: Option[omf#Entity],
 restrictedCardinality: taggedTypes.PositiveIntegerLiteral)