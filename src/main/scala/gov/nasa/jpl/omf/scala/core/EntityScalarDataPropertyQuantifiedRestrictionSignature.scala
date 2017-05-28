package gov.nasa.jpl.omf.scala.core

import java.util.UUID
import scala.Boolean

case class EntityScalarDataPropertyQuantifiedRestrictionSignature[omf <: OMF]
( uuid: UUID,
  restrictedEntity: omf#Entity,
  scalarDataProperty: omf#EntityScalarDataProperty,
  restrictedRange: omf#DataRange,
  isExistential: Boolean
)
