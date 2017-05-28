package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName

case class SynonymScalarRestrictionSignature[omf <: OMF]
( uuid: UUID,
  name: LocalName,
  iri: omf#IRI,
  restrictedRange: omf#DataRange
)
