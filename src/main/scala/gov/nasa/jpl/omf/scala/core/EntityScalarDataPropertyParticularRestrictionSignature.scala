package gov.nasa.jpl.omf.scala.core

import java.util.UUID
import scala.Predef.String

case class EntityScalarDataPropertyParticularRestrictionSignature[omf <: OMF]
( uuid: UUID,
  restrictedEntity: omf#Entity,
  scalarDataProperty: omf#EntityScalarDataProperty,
  literalValue: String )