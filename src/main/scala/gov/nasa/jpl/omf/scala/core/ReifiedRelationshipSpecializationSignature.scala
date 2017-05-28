package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ReifiedRelationshipSpecializationSignature[omf <: OMF]
( uuid: UUID,
  sub: omf#ReifiedRelationship,
  sup: omf#ReifiedRelationship
)
