package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ConceptInstanceSignature[omf <: OMF]
( uuid: UUID,
  concept: omf#Concept )
