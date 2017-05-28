package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LocalName}

import scala.collection.immutable.Set

case class DescriptionBoxSignature[omf <: OMF, +S[A] <: scala.collection.Iterable[A], I[A] <: scala.collection.Iterable[A]]
( uuid: UUID,
  name: LocalName,
  iri: omf#IRI,
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

  annotationProperties: S[AnnotationProperty],

  annotations: S[(AnnotationProperty, I[AnnotationEntry])]) {

  def importedModules
  (implicit ops: OMFOps[omf])
  : Set[omf#Module]
  = Set.empty[omf#Module] ++
    descriptionBoxRefinements.map(ops.fromDescriptionBoxRefinementAxiom(_).importedModule) ++
    closedWorldDefinitions.map(ops.fromClosedWorldDefinitionsAxiom(_).importedModule)

}
