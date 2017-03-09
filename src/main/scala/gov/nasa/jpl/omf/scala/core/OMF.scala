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

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LocalName}
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

import scala.collection.immutable.{Iterable, Map, Seq}
import scala.{Boolean, Int, Option}
import scala.Predef.String


/**
  * OMF is JPL's Ontological Modeling Framework, a pure functional interface specification
  * supporting a rigorous integratation of two widely used paradigms:
  * - modeling a system in the sense of the Object Management Group (OMG) modeling specifications
  * (e.g. the Unified Modeling Language (UML) and the Systems Modeling Language (SysML))
  * - ontologies for describing a system according to a analyzable vocabulary for creating and reasoning
  * about such descriptions in the sense of the World-Wide Web (W3C) Ontology Web Language (OWL) standard.
  *
  * The OMF interface specification involves 4 main components:
  * - OMFStore for the management of OMF models
  * - OMFIRI for identifying "things"
  * - OMFtbox for describing a conceptual model of a domain
  * - OMFabox for describing particular situations in that domain using the OMFtbox vocabulary for that domain
  */
trait OMF extends OMFstore with OMFiri with OMFtbox with OMFabox {

}

trait OMFstore {
  /**
    * The type of an OMF storage provider.
    *
    * An instance of a Store is an implicit parameter for the construction-related operations in OMFDSL.
    */
  type Store

}

/**
  * The types involved in the identification of "things" in OMF
  */
trait OMFiri {

  /**
    * An IRI identifies "things" in OMFtbox or OMFabox graphs.
    * In an OMFtbox graph, an IRI identifies ModelTerminologyGraph, ModelEntityDefinitions and ModelDataTypeDefinitions.
    * In an OMFabox graph, an IRI identifies ModelInstanceGraphs and ModelNamedIndividuals
    */
  type IRI
}

/**
  * Types for defining OMF tbox graphs specifying conceptual models of domains.
  */
trait OMFtbox {

  type Resource

  type TerminologyThing

  type TerminologyContext

  type TerminologyStatement <: TerminologyThing
  type TerminologyBoxStatement <: TerminologyStatement
  type TerminologyBundleStatement <: TerminologyStatement


  /**
    * In OMF, the specification of the conceptual model of a domain is defined in a TBox graph.
    */
  type TerminologyBox <: TerminologyThing with Resource
  type TerminologyGraph <: TerminologyBox
  type Bundle <: TerminologyBox

  type ImmutableTerminologyBox <: TerminologyBox
  type ImmutableTerminologyGraph <: ImmutableTerminologyBox with TerminologyGraph
  type ImmutableBundle <: ImmutableTerminologyBox with Bundle

  type MutableTerminologyBox <: TerminologyBox
  type MutableTerminologyGraph <: MutableTerminologyBox with TerminologyGraph
  type MutableBundle <: MutableTerminologyBox with Bundle

  type Mutable2ImmutableTerminologyMap <: Map[MutableTerminologyBox, ImmutableTerminologyBox]

  /**
    * A ModelTypeTerm is the basic unit for defining the conceptual model of a domain in an OMF ModelTerminologyGraph.
    *
    * There are 4 kinds of ModelTypeTerms:
    *
    * - ModelEntityDefinition:
    * the vocabulary for the conceptual modeling of a domain in terms of "things" that have identity semantics
    *
    * - ModelDataTypeDefinition:
    * the vocabulary for the conceptual modeling of a domain in terms of data that has value semantics
    *
    * - ModelDataRelationship:
    * binary, directed relationships whose domain is either an entity or datatype and whose range is a datatype
    *
    * - ModelEntityUnreifiedRelationship:
    * binary, directed relationships whose domain & range are entities and whose identity is the related objects
    */
  type Term <: TerminologyBoxStatement with Resource

  /**
    * A ModelEntityDefinition defines the vocabulary for the conceptual modeling
    * of a domain in terms of "things" that have an intrinsic identity semantics.
    *
    * - ModelEntityAspect
    * - ModelEntityConcept
    * - ModelEntityReifiedRelationship
    */
  type Entity <: Term

  /**
    * A ModelEntityAspect defines an abstraction that can be the superclass of other
    * ModelEntityAspects, ModelEntityConcepts or ModelEntityReifiedRelationships.
    *
    * In an OMF ABox graph, a ModelEntityAspect cannot be explicitly instantiated;
    * however, a ModelEntityConcept or ModelEntityReifiedRelationship specialization
    * of a ModelEntityAspect can be explicitly instantiated.
    */
  type Aspect <: Entity

  /**
    * A ModelEntityConcept defines a concept in the conceptual modeling of a domain.
    *
    * In an OMF ABox, each instance of a ModelEntityConcept has a unique identity.
    */
  type Concept <: Entity

  type EntityRelationship <: Term

  type UnreifiedRelationship <: EntityRelationship

  /**
    * A ModelEntityReifiedRelationship defines a binary, directed relationship in the conceptual modeling of a domain.
    *
    * The relationship domain (aka source) and range (aka target) can be any kind of ModelEntityDefinition
    */
  type ReifiedRelationship <: EntityRelationship with Entity

  /**
    * A ModelDataTypeDefinition defines the vocabulary for the conceptual modeling
    * of a domain in terms of data that has value semantics (i.e, equality).
    *
    * There are 2 kinds of ModelDataTypeDefinitions:
    * - ModelScalarDataType, an atomic datatype in the sense of XML Schema 1.1 DataTypes
    * - ModelStructuredDataType, a structured datatype with data property relationships to other ModelDataTypeDefinitions
    */
  type Datatype <: Term

  /**
    * A ModelStructuredDataType defines a structured datatype in a conceptual model
    * in the sense that a structured datatype is defined only in terms of ModelDataRelationships
    * to other ModelDataTypeDefinitions and that the value semantics of a structured datatype
    * is based on the equality of the value of its ModelDataRelationships.
    */
  type Structure <: Datatype

  type DataRange <: Datatype

  /**
    * A ModelScalarDataType defines a scalar datatype in a conceptual model
    * in the sense that a scala datatype is 'atomic' in the sense of XML Schema 1.1 DataTypes.
    *
    * A value of a scalar datatype is always represented according to its lexical representation as a string.
    * The value semantics of a ModelScalarDataType follows XML Schema 1.1 DataTypes, that is, it is simply
    * the equality of the lexical representation of a value of a ModelScalarDataType.
    *
    * @see http://www.w3.org/TR/xmlschema11-2/#anyAtomicType
    */
  type Scalar <: DataRange

  type RestrictedDataRange <: DataRange

  type BinaryScalarRestriction <: RestrictedDataRange

  type IRIScalarRestriction <: RestrictedDataRange

  type NumericScalarRestriction <: RestrictedDataRange

  type PlainLiteralScalarRestriction <: RestrictedDataRange

  type ScalarOneOfRestriction <: RestrictedDataRange

  type StringScalarRestriction <: RestrictedDataRange

  type SynonymScalarRestriction <: RestrictedDataRange

  type TimeScalarRestriction <: RestrictedDataRange

  /**
    * A ModelDataRelationship is an abstraction for a 2x2 matrix for
    * binary directed relationships whose range is a ModelDataTypeDefinition.
    *
    * One axis is the domain of the relationship:
    * - ModelDataRelationshipFromEntity
    * - ModelDataRelationshipFromStructure
    *
    * Another axis is the range of the relationship:
    * - ModelDataRelationshipToScalar
    * - ModelDataRelationshipToStructure
    */
  type DataRelationship <: Term

  /**
    * A ModelDataRelationshipFrom is the abstraction of the domain of a ModelDataRelationship:
    * - ModelDataRelationshipFromEntity
    * - ModelDataRelationshipFromStructure
    */
  type DataRelationshipDomain
  type DataRelationshipFromEntity <: DataRelationshipDomain
  type DataRelationshipFromStructure <: DataRelationshipDomain

  /**
    * A ModelDataRelationshipFrom is the abstraction of the range of a ModelDataRelationship:
    * - ModelDataRelationshipToScalar
    * - ModelDataRelationshipToStructure
    */
  type DataRelationshipRange
  type DataRelationshipToScalar <: DataRelationshipRange
  type DataRelationshipToStructure <: DataRelationshipRange

  /**
    * A ModelDataRelationshipFromEntityToScalar is a ModelDataRelationship
    * with a domain that is a ModelDataRelationshipFromEntity
    * and with a range that is a ModelDataRelationshipToScalar
    */
  type EntityScalarDataProperty <: DataRelationship
    with DataRelationshipFromEntity
    with DataRelationshipToScalar

  /**
    * A ModelDataRelationshipFromEntityToStructure is a ModelDataRelationship
    * with a domain that is a ModelDataRelationshipFromEntity
    * and with a range that is a ModelDataRelationshipToStructure
    */
  type EntityStructuredDataProperty <: DataRelationship
    with DataRelationshipFromEntity
    with DataRelationshipToStructure

  /**
    * A ModelDataRelationshipFromStructureToScalar is a ModelDataRelationship
    * with a domain that is a ModelDataRelationshipFromStructure
    * and with a range that is a ModelDataRelationshipToScalar
    */
  type ScalarDataProperty <: DataRelationship
    with DataRelationshipFromStructure
    with DataRelationshipToScalar

  /**
    * A ModelDataRelationshipFromStructureToStructure is a ModelDataRelationship
    * with a domain that is a ModelDataRelationshipFromStructure
    * and with a range that is a ModelDataRelationshipToStructure
    */
  type StructuredDataProperty <: DataRelationship
    with DataRelationshipFromStructure
    with DataRelationshipToStructure

  /**
    * A ModelTermAxiom is the abstraction for statements about
    * ModelTypeTerms in an ModelTerminologyGraph
    */
  type Axiom <: TerminologyBoxStatement

  type ScalarOneOfLiteralAxiom <: Axiom

  type TermAxiom <: Axiom

  /**
    * An EntityDefinitionRestrictionAxiom is a ModelTermAxiom assertion about
    * constraining a ModelEntityReifiedRelationship
    * for a sub-domain ModelEntityDefinition
    * to a restricted sub-range ModelEntityDefinition.
    *
    * This restriction constraint can be universal or existential.
    */
  type EntityRestrictionAxiom <: TermAxiom

  /**
    * An EntityDefinitionUniversalRestrictionAxiom is a ModelTermAxiom assertion about
    * constraining a ModelEntityReifiedRelationship
    * for objects of a sub-domain ModelEntityDefinition
    * to be related to include a sub-range ModelEntityDefinition.
    */
  type EntityExistentialRestrictionAxiom <: EntityRestrictionAxiom

  /**
    * An EntityDefinitionUniversalRestrictionAxiom is a ModelTermAxiom assertion about
    * constraining a ModelEntityReifiedRelationship
    * for objects of a sub-domain ModelEntityDefinition
    * to be related to only a restricted sub-range ModelEntityDefinition.
    */
  type EntityUniversalRestrictionAxiom <: EntityRestrictionAxiom

  type EntityScalarDataPropertyRestrictionAxiom <: TermAxiom

  type EntityScalarDataPropertyExistentialRestrictionAxiom <: EntityScalarDataPropertyRestrictionAxiom
  type EntityScalarDataPropertyParticularRestrictionAxiom <: EntityScalarDataPropertyRestrictionAxiom
  type EntityScalarDataPropertyUniversalRestrictionAxiom <: EntityScalarDataPropertyRestrictionAxiom

  type SpecializationAxiom <: TermAxiom

  /**
    * An EntityDefinitionAspectSubClassAxiom is a ModelTermAxion assertion
    * about a ModelEntityDefinition as a subclass for a superclass ModelEntityAspect
    */
  type AspectSpecializationAxiom <: SpecializationAxiom

  /**
    * An EntityConceptSubClassAxiom is a ModelTermAxiom assertion about
    * a subclass/superclass relationship between two ModelEntityConcepts
    */
  type ConceptSpecializationAxiom <: SpecializationAxiom

  /**
    * An EntityReifiedRelationshipSubClassAxiom is a ModelTermAxiom assertion about
    * a subclass/superclass relationship between two ModelEntityReifiedRelationships
    */
  type ReifiedRelationshipSpecializationAxiom <: SpecializationAxiom

  /**
    * A TerminologyGraphAxiom is the abstraction for statements about
    * ModelTerminologyGraphs
    */
  type TerminologyAxiom <: TerminologyThing

  type TerminologyBoxAxiom <: TerminologyAxiom
  type TerminologyBundleAxiom <: TerminologyAxiom

  type RootConceptTaxonomyAxiom <: TerminologyBundleStatement with ConceptTreeDisjunction

  type ConceptTreeDisjunction

  type DisjointUnionOfConceptsAxiom <: TerminologyBundleStatement
  type SpecificDisjointConceptAxiom <: DisjointUnionOfConceptsAxiom
  type AnonymousConceptTaxonomyAxiom <: DisjointUnionOfConceptsAxiom with ConceptTreeDisjunction

  type BundledTerminologyAxiom <: TerminologyBundleAxiom

  /**
    * An EntityConceptDesignationTerminologyAxiom is a ModelTermAxion assertion
    * about a ModelEntityConcept whose complete concept designation
    * is specified in a designation ModelTerminologyGraph.
    */
  type ConceptDesignationTerminologyAxiom <: TerminologyBoxAxiom

  /**
    * A TerminologyGraphDirectExtensionAxiom(extendingChild=G1, extendedParent=G1)
    * is a TerminologyGraphAxiom assertion about the vocabulary of an extending ModelTerminologyGraph G1
    * that directly extends the vocabulary of an extended ModelTerminologyGraph G2 in the sense
    * that vocabulary terms defined in G1 can be defined in terms of or as specializations or restrictions of
    * vocabulary terms defined in G2 or in another graph G3 that is directly or indirectly an extended parent of G2.
    *
    * If:
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G1, extendedParent=G2)
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G2, extendedParent=G3)
    * Then:
    * G1 extends G2,G3
    * G2 extends G3
    *
    * If:
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G1, extendedParent=G2)
    * TerminologyGraphDirectNestingParentAxiom(extendingChild=G2, nestingParent=G3)
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G3, extendedParent=G4)
    * Then:
    * G1 extends G2,G3,G4
    * G3 extends G4
    *
    * If:
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G1, extendedParent=G2a)
    * TerminologyGraphDirectNestingParentAxiom(nestedChild=G2a, nestingParent=G3)
    * TerminologyGraphDirectNestingParentAxiom(nestedChild=G2b, nestingParent=G3)
    * TerminologyGraphDirectExtensionAxiom(extendingChild=G3, extendedParent=G4)
    * Then:
    * G1 extends G2a,G3,G4
    * G3 extends G4
    */
  type TerminologyExtensionAxiom <: TerminologyBoxAxiom

  /**
    * A TerminologyGraphDirectNestingAxiom(nestingParent=G1, nestingContext=C, nestedChild=G2)
    * is a TerminologyGraphAxiom assertion about a ModelTerminologyGraph G1 that is the
    * authorization context for a ModelEntityConcept C defined in a ModelTerminologyGraph G2
    * that is also the extended parent of G1.
    *
    * Invariants:
    * fromTerminologyGraph(G1).concepts.contains(C)
    * lookupNestingAxiomsForNestingParent(G1).contains(this)
    * lookupNestingAxiomForNestedChildIfAny(G2).contains(this)
    *
    * If:
    * TerminologyGraphDirectNestingAxiom(nestedChild=G1, nestingParent=G2)
    * TerminologyGraphDirectNestingAxiom(nestedChild=G2, nestingParent=G3)
    * Then:
    * G1 has nesting parents G2,G3
    * G2 has nesting parents G3
    * G2 has nested children G1
    * G3 has nested children G1,G2
    */
  type TerminologyNestingAxiom <: TerminologyBoxAxiom
}

/**
  * Types for defining OMF abox graphs for describing domain-specific situations according
  * to the OMF tbox graphs defining the conceptual models of these domains.
  */
trait OMFabox {

  /**
    * In OMF, the description of a particular domain-specific situation is defined in an ABox graph
    * for the OMF TBox graph of that domain.
    */
  type ModelInstanceGraph
  type ImmutableModelInstanceGraph <: ModelInstanceGraph
  type MutableModelInstanceGraph <: ModelInstanceGraph

  /**
    * A ModelInstanceAssertion is the basic unit for describing a domain-specific situation in an OMF ABox graph.
    *
    * There are
    * - ModelInstanceRelationshipAssertion:
    * Asserts an ModelEntityUnreifiedRelationship between ModelNamedIndividuals
    *
    * - ModelNamedIndividual:
    * Identifies an instance of a ModelEntityConcept, ModelEntityReifiedRelationship,
    * ModelStructuredDataType, ModelInstanceDataRelationshipFromEntityToStructure,
    * ModelInstanceDataRelationshipFromStructureToStructure
    *
    * - ModelInstanceDataRelationshipFromEntityToScalar:
    * Asserts the scalar value of a data property relationship for a ModelEntityInstance
    *
    * - ModelInstanceDataRelationshipFromStructureToScalar:
    * Asserts the scalar value of a data property relationship for a ModelStructuredDataType
    */
  type ModelInstanceAssertion

  /**
    * A ModelInstanceRelationshipAssertion asserts an ModelEntityUnreifiedRelationship between ModelNamedIndividuals
    */
  type ModelInstanceRelationshipAssertion <: ModelInstanceAssertion

  /**
    * A ModelNamedIndividual identifies an instance of a ModelEntityConcept, ModelEntityReifiedRelationship,
    * ModelStructuredDataType, ModelInstanceDataRelationshipFromEntityToStructure,
    * ModelInstanceDataRelationshipFromStructureToStructure
    */
  type ModelNamedIndividual <: ModelInstanceAssertion

  /**
    * A ModelEntityInstance identifies an instance of a ModelEntityConcept or ModelEntityReifiedRelationship
    */
  type ModelEntityInstance <: ModelNamedIndividual

  /**
    * A ModelInstanceObject identifies an isntance of a ModelEntityConcept
    */
  type ModelInstanceObject <: ModelEntityInstance

  /**
    * A ModelInstanceRelation identifies an instance of a ModelEntityReifiedRelationship
    */
  type ModelInstanceRelation <: ModelEntityInstance

  /**
    * A ModelDataInstance is a value of a ModelDataTypeDefinition
    */
  type ModelDataInstance

  /**
    * A ModelInstanceDataLiteral is a value of a ModelScalarDataType
    */
  type ModelInstanceDataLiteral <: ModelInstanceAssertion with ModelDataInstance

  /**
    * A ModelInstanceDataStructure is a value of a ModelStructuredDataType
    */
  type ModelInstanceDataStructure <: ModelNamedIndividual with ModelDataInstance

  /**
    * A ModelInstanceDataRelationshipFromEntityToScalar asserts
    * the scalar value of a ModelDataRelationshipFromEntityToScalar property relationship
    * for a ModelEntityInstance
    */
  type ModelInstanceDataRelationshipFromEntityToScalar <: ModelInstanceAssertion

  /**
    * A ModelInstanceDataRelationshipFromEntityToStructure asserts
    * the structured value of a ModelDataRelationshipFromEntityToStructure property relationship
    * for a ModelEntityInstance
    */
  type ModelInstanceDataRelationshipFromEntityToStructure <: ModelNamedIndividual with ModelInstanceAssertion

  /**
    * A ModelInstanceDataRelationshipFromStructureToScalar asserts
    * the scalar value of a ModelDataRelationshipFromStructureToScalar property relationship
    * for a ModelInstanceDataStructure
    */
  type ModelInstanceDataRelationshipFromStructureToScalar <: ModelInstanceAssertion

  /**
    * A ModelInstanceDataRelationshipFromStructureToStructure asserts
    * the structured value of a ModelDataRelationshipFromStructureToStructure property relationship
    * for a ModelInstanceDataStructure
    */
  type ModelInstanceDataRelationshipFromStructureToStructure <: ModelNamedIndividual with ModelInstanceAssertion

}


/**
  * A Terminology signature is a tuple.
  *
  * @tparam omf OMF Adaptation/Binding.
  */
trait TerminologySignature[omf <: OMF] {
  val isBundle: Boolean
  val uuid: UUID
  val name: LocalName
  /**
    * the identity of the terminology as a container for several descriptions and as the context
    * for extending other terminologies
    */
  val iri: omf#IRI
  /**
    * the semantic commitment of this terminology (open-world definitions vs. closed-world designations)
    */
  val kind: TerminologyKind
  /**
    * this terminology graph can use or specialize vocabulary terms
    * defined in the transitive closure of imported terminology graphs
    */
  val imports: Iterable[omf#TerminologyBox]
  /**
    * this terminology can be a nested terminology for a parent concept in a parent terminology
    */
  val nesting: Option[omf#Concept]
  /**
    * the aspects described in this terminology
    */
  val aspects: Iterable[omf#Aspect]
  /**
    * the concepts described in this terminology
    */
  val concepts: Iterable[omf#Concept]
  /**
    * the reified relationships described in this terminology
    */
  val reifiedRelationships: Iterable[omf#ReifiedRelationship]
  /**
    * the unreified relationships described in scope of this terminology
    */
  val unreifiedRelationships: Iterable[omf#UnreifiedRelationship]
  /**
    * the scalar datatypes described in this terminology
    */
  val scalarDataTypes: Iterable[omf#Scalar]
  /**
    * the structured datatypes described in this terminology
    */
  val structuredDataTypes: Iterable[omf#Structure]

  val scalarOneOfRestrictions: Iterable[omf#ScalarOneOfRestriction]
  val binaryScalarRestrictions: Iterable[omf#BinaryScalarRestriction]
  val iriScalarRestrictions: Iterable[omf#IRIScalarRestriction]
  val numericScalarRestrictions: Iterable[omf#NumericScalarRestriction]
  val plainLiteralScalarRestrictions: Iterable[omf#PlainLiteralScalarRestriction]
  val stringScalarRestrictions: Iterable[omf#StringScalarRestriction]
  val synonymScalarRestrictions: Iterable[omf#SynonymScalarRestriction]
  val timeScalarRestrictions: Iterable[omf#TimeScalarRestriction]

  /**
    * the entity to scalar data relationships described in this terminology
    */
  val entityScalarDataProperties: Iterable[omf#EntityScalarDataProperty]
  /**
    * the entity to structured data relationships described in this terminology
    */
  val entityStructuredDataProperties: Iterable[omf#EntityStructuredDataProperty]
  /**
    * the entity to scalar data  relationships described in this terminology
    */
  val scalarDataProperties: Iterable[omf#ScalarDataProperty]
  /**
    * the entity to scalar data  relationships described in this terminology
    */
  val structuredDataProperties: Iterable[omf#StructuredDataProperty]

  val terms: Iterable[omf#Term]
  = aspects ++
    concepts ++
    reifiedRelationships ++
    unreifiedRelationships ++
    scalarDataTypes ++
    structuredDataTypes ++
    scalarOneOfRestrictions ++
    binaryScalarRestrictions ++
    iriScalarRestrictions ++
    plainLiteralScalarRestrictions ++
    stringScalarRestrictions ++
    timeScalarRestrictions ++
    entityScalarDataProperties ++
    entityStructuredDataProperties ++
    scalarDataProperties ++
    structuredDataProperties

  /**
    * the model term axioms asserted in this terminology
    */
  val axioms: Iterable[omf#Axiom]
  /**
    * The terminology graph axioms asserted in this terminology
    */
  val gaxioms: Iterable[omf#TerminologyBoxAxiom]

  val bAxioms: Iterable[omf#BundledTerminologyAxiom]
  val rTAxioms: Iterable[omf#RootConceptTaxonomyAxiom]
  val aTAxioms: Iterable[omf#AnonymousConceptTaxonomyAxiom]
  val sTAxioms: Iterable[omf#SpecificDisjointConceptAxiom]

  val annotations: Map[AnnotationProperty, Seq[AnnotationEntry]]
}


trait ConceptDesignationTerminologySignature[omf <: OMF] {
  val uuid: UUID
  val parentConcept: omf#Concept
  val parentGraph: omf#TerminologyBox
}

trait TerminologyExtensionSignature[omf <: OMF] {
  val uuid: UUID
  val extendedTerminology: omf#TerminologyBox
}

trait TerminologyNestingSignature[omf <: OMF] {
  val uuid: UUID
  val nestingTerminology: omf#TerminologyBox
  val nestingContext: omf#Concept
}

trait EntityConceptSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val isAbstract: Boolean
}

trait EntityReifiedRelationshipSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val unreifiedPropertyName: LocalName
  val unreifiedInversePropertyName: Option[LocalName]
  val iri: omf#IRI
  val source: omf#Entity
  val target: omf#Entity
  val characteristics: Iterable[RelationshipCharacteristics]
  val isAbstract: Boolean
}

trait EntityUnreifiedRelationshipSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val source: omf#Entity
  val target: omf#Entity
  val characteristics: Iterable[RelationshipCharacteristics]
}

trait EntityScalarDataPropertySignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val domain: omf#Entity
  val range: omf#DataRange
  val isIdentityCriteria: Boolean
}

trait EntityStructuredDataPropertySignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val domain: omf#Entity
  val range: omf#Structure
  val isIdentityCriteria: Boolean
}

trait ScalarDataPropertySignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val domain: omf#Structure
  val range: omf#DataRange
}

trait StructuredDataPropertySignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val domain: omf#Structure
  val range: omf#Structure
}

trait AspectSpecializationSignature[omf <: OMF] {
  val uuid: UUID
  val sub: omf#Entity
  val sup: omf#Aspect
}

trait ConceptSpecializationSignature[omf <: OMF] {
  val uuid: UUID
  val sub: omf#Concept
  val sup: omf#Concept
}

trait ReifiedRelationshipSpecializationSignature[omf <: OMF] {
  val uuid: UUID
  val sub: omf#ReifiedRelationship
  val sup: omf#ReifiedRelationship
}

trait EntityRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val domain: omf#Entity
  val restrictedRelation: omf#ReifiedRelationship
  val range: omf#Entity
}

trait EntityScalarDataPropertyQuantifiedRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val restrictedEntity: omf#Entity
  val scalarDataProperty: omf#EntityScalarDataProperty
  val restrictedRange: omf#DataRange
}

trait EntityScalarDataPropertyParticularRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val restrictedEntity: omf#Entity
  val scalarDataProperty: omf#EntityScalarDataProperty
  val literalValue: String
}

trait ScalarOneOfLiteralSignature[omf <: OMF] {
  val uuid: UUID
  val restriction: omf#ScalarOneOfRestriction
  val value: String
}

trait BinaryScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val length: Option[Int]
  val minLength: Option[Int]
  val maxLength: Option[Int]
  val restrictedRange: omf#DataRange
}

trait IRIScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val length: Option[Int]
  val minLength: Option[Int]
  val maxLength: Option[Int]
  val pattern: Option[String]
  val restrictedRange: omf#DataRange
}

trait NumericScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val minInclusive: Option[String]
  val maxInclusive: Option[String]
  val minExclusive: Option[String]
  val maxExclusive: Option[String]
  val restrictedRange: omf#DataRange
}

trait PlainLiteralScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val length: Option[Int]
  val minLength: Option[Int]
  val maxLength: Option[Int]
  val pattern: Option[String]
  val langRange: Option[String]
  val restrictedRange: omf#DataRange
}

trait ScalarOneOfRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val restrictedRange: omf#DataRange
}

trait StringScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val length: Option[Int]
  val minLength: Option[Int]
  val maxLength: Option[Int]
  val pattern: Option[String]
  val restrictedRange: omf#DataRange
}

trait SynonymScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val restrictedRange: omf#DataRange
}

trait TimeScalarRestrictionSignature[omf <: OMF] {
  val uuid: UUID
  val name: LocalName
  val iri: omf#IRI
  val minInclusive: Option[String]
  val maxInclusive: Option[String]
  val minExclusive: Option[String]
  val maxExclusive: Option[String]
  val restrictedRange: omf#DataRange
}

trait BundledTerminologySignature[omf <: OMF] {
  val uuid: UUID
  val bundle: omf#Bundle
  val bundledTerminology: omf#TerminologyBox
}

trait AnonymousConceptTaxonomySignature[omf <: OMF] {
  val uuid: UUID
  val bundle: omf#Bundle
  val disjointTaxonomyParent: omf#ConceptTreeDisjunction
}

trait RootConceptTaxonomySignature[omf <: OMF] {
  val uuid: UUID
  val bundle: omf#Bundle
  val root: omf#Concept
}

trait SpecificDisjointConceptSignature[omf <: OMF] {
  val uuid: UUID
  val bundle: omf#Bundle
  val disjointTaxonomyParent: omf#ConceptTreeDisjunction
  val disjointLeaf: omf#Concept
}
