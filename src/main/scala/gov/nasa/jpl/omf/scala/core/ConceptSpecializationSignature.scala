package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ConceptSpecializationSignature[omf <: OMF]
( uuid: UUID,
  sub: omf#Concept,
  sup: omf#Concept
)
