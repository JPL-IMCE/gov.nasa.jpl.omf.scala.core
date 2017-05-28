package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class RootConceptTaxonomySignature[omf <: OMF]
( uuid: UUID,
  bundle: omf#Bundle,
  root: omf#Concept
)
