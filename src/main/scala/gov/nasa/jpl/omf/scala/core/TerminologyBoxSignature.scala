/*
 * Copyright 2015 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue, LocalName}

import scala.collection.immutable.Set
import scala.Boolean

/**
  * Information about the contents of a TerminologyBox
  *
  * @param isBundle
  * @param uuid
  * @param name
  * @param iri identity of the terminology
  * @param kind semantic commitment of this terminology (open-world definitions vs. closed-world designations)
  * @param extensions
  * @param nesting
  * @param conceptDesignation
  * @param bundledTerminologies
  * @param aspects
  * @param concepts
  * @param reifiedRelationships
  * @param unreifiedRelationships
  * @param scalarDataTypes
  * @param structuredDataTypes
  * @param scalarOneOfRestrictions
  * @param scalarOneOfLiterals
  * @param binaryScalarRestrictions
  * @param iriScalarRestrictions
  * @param numericScalarRestrictions
  * @param plainLiteralScalarRestrictions
  * @param stringScalarRestrictions
  * @param synonymScalarRestrictions
  * @param timeScalarRestrictions
  * @param entityScalarDataProperties
  * @param entityStructuredDataProperties
  * @param scalarDataProperties
  * @param structuredDataProperties
  * @param axioms
  * @param rTAxioms
  * @param aTAxioms
  * @param sTAxioms
  * @param bAxioms
  * @param annotationPropertyValues
  * @param annotationProperties
  * @tparam omf OMF Adaptation/Binding.
  * @tparam S A container type (either scala.collection.immutable.Set or scala.collection.mutable.HashSet)
  */
case class TerminologyBoxSignature[omf <: OMF, +S[A] <: scala.collection.Iterable[A]]
( isBundle: Boolean,
  override val uuid: UUID,
  override val name: LocalName,

  override val iri: omf#IRI,
  kind: TerminologyKind,

  extensions: S[omf#TerminologyExtensionAxiom],
  nesting: S[omf#TerminologyNestingAxiom],
  conceptDesignation: S[omf#ConceptDesignationTerminologyAxiom],
  bundledTerminologies: S[omf#BundledTerminologyAxiom],

  aspects: S[omf#Aspect],
  concepts: S[omf#Concept],
  reifiedRelationships: S[omf#ReifiedRelationship],
  unreifiedRelationships: S[omf#UnreifiedRelationship],
  scalarDataTypes: S[omf#Scalar],
  structuredDataTypes: S[omf#Structure],

  scalarOneOfRestrictions: S[omf#ScalarOneOfRestriction],
  scalarOneOfLiterals: S[omf#ScalarOneOfLiteralAxiom],

  binaryScalarRestrictions: S[omf#BinaryScalarRestriction],
  iriScalarRestrictions: S[omf#IRIScalarRestriction],
  numericScalarRestrictions: S[omf#NumericScalarRestriction],
  plainLiteralScalarRestrictions: S[omf#PlainLiteralScalarRestriction],
  stringScalarRestrictions: S[omf#StringScalarRestriction],
  synonymScalarRestrictions: S[omf#SynonymScalarRestriction],
  timeScalarRestrictions: S[omf#TimeScalarRestriction],

  entityScalarDataProperties: S[omf#EntityScalarDataProperty],
  entityStructuredDataProperties: S[omf#EntityStructuredDataProperty],
  scalarDataProperties: S[omf#ScalarDataProperty],
  structuredDataProperties: S[omf#StructuredDataProperty],

  restrictionStructuredDataPropertyTuples: S[omf#RestrictionStructuredDataPropertyTuple],
  restrictionScalarDataPropertyValues: S[omf#RestrictionScalarDataPropertyValue],

  chainRules: S[omf#ChainRule],
  ruleBodySegments: S[omf#RuleBodySegment],
  aspectPredicates: S[omf#AspectPredicate],
  conceptPredicates: S[omf#ConceptPredicate],
  reifiedRelationshipPredicates: S[omf#ReifiedRelationshipPredicate],

  reifiedRelationshipPropertyPredicates: S[omf#ReifiedRelationshipPropertyPredicate],
  reifiedRelationshipInversePropertyPredicates: S[omf#ReifiedRelationshipInversePropertyPredicate],

  reifiedRelationshipSourcePropertyPredicates: S[omf#ReifiedRelationshipSourcePropertyPredicate],
  reifiedRelationshipSourceInversePropertyPredicates: S[omf#ReifiedRelationshipSourceInversePropertyPredicate],

  reifiedRelationshipTargetPropertyPredicates: S[omf#ReifiedRelationshipTargetPropertyPredicate],
  reifiedRelationshipTargetInversePropertyPredicates: S[omf#ReifiedRelationshipTargetInversePropertyPredicate],

  unreifiedRelationshipPropertyPredicates: S[omf#UnreifiedRelationshipPropertyPredicate],
  unreifiedRelationshipInversePropertyPredicates: S[omf#UnreifiedRelationshipInversePropertyPredicate],

  axioms: S[omf#Axiom],

  rTAxioms: S[omf#RootConceptTaxonomyAxiom],
  aTAxioms: S[omf#AnonymousConceptTaxonomyAxiom],
  sTAxioms: S[omf#SpecificDisjointConceptAxiom],
  bAxioms: S[omf#BundledTerminologyAxiom],

  override val annotationPropertyValues: S[AnnotationPropertyValue],
  override val annotationProperties: S[AnnotationProperty])
  extends ModuleSignature[omf] {

  override def importedTerminologies
  (implicit ops: OMFOps[omf])
  : Set[omf#TerminologyBox]
  = Set.empty[omf#TerminologyBox] ++
    extensions.map(ops.fromTerminologyExtensionAxiom(_).importedTerminologyBox) ++
    conceptDesignation.map(ops.fromConceptDesignationTerminologyAxiom(_).importedTerminologyBox) ++
    bundledTerminologies.map(ops.fromBundledTerminologyAxiom(_).importedTerminologyBox)

  override def importedDescriptions
  (implicit ops: OMFOps[omf])
  : Set[omf#DescriptionBox]
  = Set.empty

  def terms
  : Set[omf#Term]
  = Set.empty[omf#Term] ++
      aspects ++
      concepts ++
      reifiedRelationships ++
      scalarDataTypes ++
      structuredDataTypes ++
      scalarOneOfRestrictions ++
      binaryScalarRestrictions ++
      plainLiteralScalarRestrictions ++
      stringScalarRestrictions ++
      synonymScalarRestrictions ++
      timeScalarRestrictions ++
      entityScalarDataProperties ++
      entityStructuredDataProperties ++
      scalarDataProperties ++
      structuredDataProperties ++
      chainRules

}
