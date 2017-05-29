package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LocalName}

import scala.collection.immutable.Set

trait ModuleSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  /**
    * the identity of the terminology as a container for several descriptions and as the context
    * for extending other terminologies
    */
  val iri: omf#IRI
  val annotationProperties: scala.collection.Iterable[AnnotationProperty]

  val annotations: scala.collection.Iterable[(AnnotationProperty, scala.collection.Iterable[AnnotationEntry])]

  def importedTerminologies
  (implicit ops: OMFOps[omf])
  : Set[omf#TerminologyBox]

  def importedDescriptions
  (implicit ops: OMFOps[omf])
  : Set[omf#DescriptionBox]
}
