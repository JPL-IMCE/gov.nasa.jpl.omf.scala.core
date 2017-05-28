package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics

import scala.collection.immutable.Iterable
import scala.Option

case class ReifiedRelationshipSignature[omf <: OMF]
(uuid: UUID,
 name: LocalName,
 unreifiedPropertyName: LocalName,
 unreifiedInversePropertyName: Option[LocalName],
 iri: omf#IRI,
 source: omf#Entity,
 target: omf#Entity,
 characteristics: Iterable[RelationshipCharacteristics])
