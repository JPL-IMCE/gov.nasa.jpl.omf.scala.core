package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName
import scala.Boolean

case class EntityStructuredDataPropertySignature[omf <: OMF]
( uuid: UUID,
  name: LocalName,
  iri: omf#IRI,
  domain: omf#Entity,
  range: omf#Structure,
  isIdentityCriteria: Boolean
)
