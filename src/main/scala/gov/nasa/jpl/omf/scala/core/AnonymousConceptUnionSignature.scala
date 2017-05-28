package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.LocalName

case class AnonymousConceptUnionSignature[omf <: OMF]
( uuid: UUID,
  name: LocalName,
  bundle: omf#Bundle,
  disjointTaxonomyParent: omf#ConceptTreeDisjunction
)
