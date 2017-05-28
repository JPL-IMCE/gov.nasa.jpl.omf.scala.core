package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.omf.scala.core.OMLString.LexicalValue

case class ScalarDataPropertyValueSignature[omf <: OMF]
( uuid: UUID,
  singletonInstanceStructuredDataPropertyContextUUID: omf#SingletonInstanceStructuredDataPropertyContext,
  scalarDataProperty: omf#DataRelationshipToScalar,
  scalarPropertyValue: LexicalValue)
