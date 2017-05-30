package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LocalName}

import scala.collection.immutable.Set

case class DescriptionBoxSignature[omf <: OMF, +S[A] <: scala.collection.Iterable[A]]
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: omf#IRI,
  kind: DescriptionKind,

  descriptionBoxRefinements: S[omf#DescriptionBoxRefinement],
  closedWorldDefinitions: S[omf#DescriptionBoxExtendsClosedWorldDefinitions],
  conceptInstances: S[omf#ConceptInstance],
  reifiedRelationshipInstances: S[omf#ReifiedRelationshipInstance],
  reifiedRelationshipInstanceDomains: S[omf#ReifiedRelationshipInstanceDomain],
  reifiedRelationshipInstanceRanges: S[omf#ReifiedRelationshipInstanceRange],
  unreifiedRelationshipInstanceTuples: S[omf#UnreifiedRelationshipInstanceTuple],
  singletonScalarDataPropertyValues: S[omf#SingletonInstanceScalarDataPropertyValue],
  singletonStructuredDataPropertyValues: S[omf#SingletonInstanceStructuredDataPropertyValue],
  scalarDataPropertyValues: S[omf#ScalarDataPropertyValue],
  structuredDataPropertyTuples: S[omf#StructuredDataPropertyTuple],

  override val annotationProperties: S[AnnotationProperty],

  override val annotations: S[(AnnotationProperty, scala.collection.immutable.Set[AnnotationEntry])])
  extends ModuleSignature[omf]  {

  override def importedTerminologies
  (implicit ops: OMFOps[omf])
  : Set[omf#TerminologyBox]
  = Set.empty[omf#TerminologyBox] ++
    closedWorldDefinitions.map(ops.fromClosedWorldDefinitionsAxiom(_).extendedClosedWorldDefinitions)

  override def importedDescriptions
  (implicit ops: OMFOps[omf])
  : Set[omf#DescriptionBox]
  = Set.empty[omf#DescriptionBox] ++
    descriptionBoxRefinements.map(ops.fromDescriptionBoxRefinementAxiom(_).refinedDescriptionBox)

}
