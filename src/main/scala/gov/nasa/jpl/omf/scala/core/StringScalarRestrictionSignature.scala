package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName
import scala.{Int,Option}
import scala.Predef.String

case class StringScalarRestrictionSignature[omf <: OMF]
( uuid: UUID,
  name: LocalName,
  iri: omf#IRI,
  length: Option[Int],
  minLength: Option[Int],
  maxLength: Option[Int],
  pattern: Option[String],
  restrictedRange: omf#DataRange
)
