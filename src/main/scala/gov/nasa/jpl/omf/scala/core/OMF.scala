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
trait OMF[omf <: OMF[omf]]
  extends OMFstore
    with OMFiri
    with OMFtbox
    with OMFdbox {

  type OntologyMapping <: Mutable2ImmutableModuleTable[omf]

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

// Corresponds to OMLCommon.xcore
trait OMFcbox {

  type LogicalElement
  type Resource
  type Predicate <: Resource
  type RestrictableRelationship <: Predicate
  type ModuleElement <: LogicalElement
  type ModuleEdge <: LogicalElement

  type AnnotationProperty
  type Annotation
  type Module <: LogicalElement with Resource
  type ImmutableModule <: Module
  type MutableModule <: Module

}

/**
  * Types for defining OMF tbox graphs specifying conceptual models of domains.
  */
trait OMFtbox extends OMFcbox {

  type TerminologyBoxStatement <: ModuleElement
  type TerminologyBundleStatement <: ModuleElement


  /**
    * In OMF, the specification of the conceptual model of a domain is defined in a TBox graph.
    */
  type TerminologyBox <: Module
  type TerminologyGraph <: TerminologyBox
  type Bundle <: TerminologyBox

  type ImmutableTerminologyBox <: TerminologyBox with ImmutableModule
  type ImmutableTerminologyGraph <: ImmutableTerminologyBox with TerminologyGraph
  type ImmutableBundle <: ImmutableTerminologyBox with Bundle

  type MutableTerminologyBox <: TerminologyBox with MutableModule
  type MutableTerminologyGraph <: MutableTerminologyBox with TerminologyGraph
  type MutableBundle <: MutableTerminologyBox with Bundle

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
  type Entity <: Term with Predicate

  type AspectKind <: Entity
  type Aspect <: AspectKind
  type CardinalityRestrictedAspect <: AspectKind

  type ConceptualEntity <: Entity
  type ConceptKind <: ConceptualEntity
  type Concept <: ConceptKind
  type CardinalityRestrictedConcept <: ConceptKind

  type EntityRelationship <: Term

  type CharacterizedEntityRelationship <: EntityRelationship

  type ConceptualRelationship <: ConceptualEntity with EntityRelationship

  type UnreifiedRelationship <: CharacterizedEntityRelationship with RestrictableRelationship

  type ReifiedRelationship <: CharacterizedEntityRelationship with ConceptualRelationship

  type ReifiedRelationshipRestriction <: ConceptualRelationship

  type CardinalityRestrictedReifiedRelationship <: ConceptualRelationship

  type ForwardProperty <: RestrictableRelationship with LogicalElement

  type InverseProperty <: RestrictableRelationship with LogicalElement

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
  type DataRelationshipDomain <: Term
  type DataRelationshipFromEntity <: DataRelationshipDomain
  type DataRelationshipFromStructure <: DataRelationshipDomain

  /**
    * A ModelDataRelationshipFrom is the abstraction of the range of a ModelDataRelationship:
    * - ModelDataRelationshipToScalar
    * - ModelDataRelationshipToStructure
    */
  type DataRelationshipRange <: Term
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

  type Rule <: Term
  type ChainRule <: Rule

  type RuleBodySegment <: LogicalElement
  type SegmentPredicate <: LogicalElement

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

  type EntityStructuredDataPropertyRestrictionAxiom <: TermAxiom

  type RestrictionStructuredDataPropertyContext <: ModuleElement

  type EntityStructuredDataPropertyParticularRestrictionAxiom <:
    EntityStructuredDataPropertyRestrictionAxiom with RestrictionStructuredDataPropertyContext

  type RestrictionStructuredDataPropertyTuple <: RestrictionStructuredDataPropertyContext

  type RestrictionScalarDataPropertyValue <: LogicalElement

  type SpecializationAxiom <: TermAxiom

  type AspectSpecializationAxiom <: SpecializationAxiom

  type ConceptSpecializationAxiom <: SpecializationAxiom

  type ReifiedRelationshipSpecializationAxiom <: SpecializationAxiom

  type SubObjectPropertyOfAxiom <: TermAxiom

  type SubDataPropertyOfAxiom <: TermAxiom

  /**
    * A TerminologyGraphAxiom is the abstraction for statements about
    * ModelTerminologyGraphs
    */
  type TerminologyAxiom <: ModuleEdge

  type TerminologyBoxAxiom <: TerminologyAxiom
  type TerminologyBundleAxiom <: TerminologyAxiom

  type RootConceptTaxonomyAxiom <: TerminologyBundleStatement with ConceptTreeDisjunction

  type ConceptTreeDisjunction <: LogicalElement

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
  * Types for defining OMF dbox graphs for describing domain-specific situations according
  * to the OMF tbox graphs defining the conceptual models of these domains.
  */
trait OMFdbox extends OMFcbox {

  type DescriptionBox <: Module

  type ImmutableDescriptionBox <: DescriptionBox with ImmutableModule

  type MutableDescriptionBox <: DescriptionBox with MutableModule

  type DescriptionBoxRelationship <: ModuleEdge

  type DescriptionBoxExtendsClosedWorldDefinitions <: DescriptionBoxRelationship

  type DescriptionBoxRefinement <: DescriptionBoxRelationship

  type TerminologyInstanceAssertion <: ModuleElement

  type SingletonInstanceScalarDataPropertyValue <: ModuleElement

  type SingletonInstanceStructuredDataPropertyContext <: LogicalElement

  type SingletonInstanceStructuredDataPropertyValue <: SingletonInstanceStructuredDataPropertyContext with ModuleElement

  type StructuredDataPropertyTuple <: SingletonInstanceStructuredDataPropertyContext

  type ScalarDataPropertyValue <: LogicalElement

  type ConceptualEntitySingletonInstance <: TerminologyInstanceAssertion with Resource

  type ConceptInstance <: ConceptualEntitySingletonInstance

  type ReifiedRelationshipInstance <: ConceptualEntitySingletonInstance

  type ReifiedRelationshipInstanceDomain <: TerminologyInstanceAssertion

  type ReifiedRelationshipInstanceRange <: TerminologyInstanceAssertion

  type UnreifiedRelationshipInstanceTuple <: TerminologyInstanceAssertion

  type InstanceRelationshipEnumerationRestriction <: TerminologyInstanceAssertion

  type InstanceRelationshipExistentialRangeRestriction <: TerminologyInstanceAssertion

  type InstanceRelationshipUniversalRangeRestriction <: TerminologyInstanceAssertion

  type InstanceRelationshipValueRestriction <: TerminologyInstanceAssertion
}




