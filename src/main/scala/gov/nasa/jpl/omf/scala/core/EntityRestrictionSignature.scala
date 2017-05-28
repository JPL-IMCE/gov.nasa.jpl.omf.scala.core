package gov.nasa.jpl.omf.scala.core

import java.util.UUID
import scala.Boolean

case class EntityRestrictionSignature[omf <: OMF]
( uuid: UUID,
  domain: omf#Entity,
  restrictedRelation: omf#ReifiedRelationship,
  range: omf#Entity,
  isExistential: Boolean
)
