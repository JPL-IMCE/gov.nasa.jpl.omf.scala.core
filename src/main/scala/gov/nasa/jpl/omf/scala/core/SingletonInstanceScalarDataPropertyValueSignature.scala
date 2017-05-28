package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.omf.scala.core.OMLString.LexicalValue

case class SingletonInstanceScalarDataPropertyValueSignature[omf <: OMF]
( uuid: UUID,
  singletonInstance: omf#ConceptualEntitySingletonInstance,
  scalarDataProperty: omf#EntityScalarDataProperty,
  scalarDataPropertyValue: LexicalValue)

