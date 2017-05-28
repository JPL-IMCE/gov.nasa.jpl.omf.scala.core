package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class SpecificDisjointConceptSignature[omf <: OMF]
(uuid: UUID,
 bundle: omf#Bundle,
 disjointTaxonomyParent: omf#ConceptTreeDisjunction,
 disjointLeaf: omf#Concept)
