package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName
import scala.Option
import scala.Predef.String

case class TimeScalarRestrictionSignature[omf <: OMF]
( uuid: UUID,
  name: LocalName,
  iri: omf#IRI,
  minInclusive: Option[String],
  maxInclusive: Option[String],
  minExclusive: Option[String],
  maxExclusive: Option[String],
  restrictedRange: omf#DataRange
)
