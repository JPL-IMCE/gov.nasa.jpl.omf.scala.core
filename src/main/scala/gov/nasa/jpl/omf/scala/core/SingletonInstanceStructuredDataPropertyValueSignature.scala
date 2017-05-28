package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class SingletonInstanceStructuredDataPropertyValueSignature[omf <: OMF]
( uuid: UUID,
  singletonInstance: omf#ConceptualEntitySingletonInstance,
  structuredDataProperty: omf#EntityStructuredDataProperty)
