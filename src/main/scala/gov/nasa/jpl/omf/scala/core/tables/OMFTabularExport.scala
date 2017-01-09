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

package gov.nasa.jpl.omf.scala.core.tables

import gov.nasa.jpl.imce.omf.schema.tables.{annotationOrdering, Annotation}
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.imce.omf._

import scala.collection.immutable.{Seq,Set,SortedSet}
import scala.{Option,None,Some}
import scala.Predef.{ArrowAssoc,String}

case class Axioms
( aspectSpecializationAxioms : Seq[schema.tables.AspectSpecializationAxiom] = Seq.empty,
  conceptSpecializationAxioms : Seq[schema.tables.ConceptSpecializationAxiom] = Seq.empty,
  reifiedRelationshipSpecializationAxioms : Seq[schema.tables.ReifiedRelationshipSpecializationAxiom] = Seq.empty,

  entityExistentialRestrictionAxioms : Seq[schema.tables.EntityExistentialRestrictionAxiom] = Seq.empty,
  entityUniversalRestrictionAxioms : Seq[schema.tables.EntityUniversalRestrictionAxiom] = Seq.empty,

  entityScalarDataPropertyExistentialRestrictionAxioms : Seq[schema.tables.EntityScalarDataPropertyExistentialRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyParticularRestrictionAxioms : Seq[schema.tables.EntityScalarDataPropertyParticularRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyUniversalRestrictionAxioms : Seq[schema.tables.EntityScalarDataPropertyUniversalRestrictionAxiom] = Seq.empty,

  scalarOneOfLiteralAxioms : Seq[schema.tables.ScalarOneOfLiteralAxiom] = Seq.empty )

object Axioms {

  def append
  (a1: Axioms, a2: Axioms)
  : Axioms
  = a1.copy(
    aspectSpecializationAxioms =
      a1.aspectSpecializationAxioms ++ a2.aspectSpecializationAxioms,
    conceptSpecializationAxioms =
      a1.conceptSpecializationAxioms ++ a2.conceptSpecializationAxioms,
    reifiedRelationshipSpecializationAxioms =
      a1.reifiedRelationshipSpecializationAxioms ++ a2.reifiedRelationshipSpecializationAxioms,

    entityExistentialRestrictionAxioms =
      a1.entityExistentialRestrictionAxioms ++ a2.entityExistentialRestrictionAxioms,
    entityUniversalRestrictionAxioms =
      a1.entityUniversalRestrictionAxioms ++ a2.entityUniversalRestrictionAxioms,

    entityScalarDataPropertyExistentialRestrictionAxioms =
      a1.entityScalarDataPropertyExistentialRestrictionAxioms ++ a2.entityScalarDataPropertyExistentialRestrictionAxioms,
    entityScalarDataPropertyParticularRestrictionAxioms =
      a1.entityScalarDataPropertyParticularRestrictionAxioms ++ a2.entityScalarDataPropertyParticularRestrictionAxioms,
    entityScalarDataPropertyUniversalRestrictionAxioms =
      a1.entityScalarDataPropertyUniversalRestrictionAxioms ++ a2.entityScalarDataPropertyUniversalRestrictionAxioms,

    scalarOneOfLiteralAxioms =
      a1.scalarOneOfLiteralAxioms ++ a2.scalarOneOfLiteralAxioms
  )

  def funAspectSpecializationAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#AspectSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromAspectSubClassAxiom(ax)
    acc.copy(aspectSpecializationAxioms = acc.aspectSpecializationAxioms :+
      schema.tables.AspectSpecializationAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        subEntityUUID = ops.getTermUUID(info.sub).toString,
        superAspectUUID = ops.getTermUUID(info.sup).toString))
  }

  def funConceptSpecializationAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ConceptSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromConceptSpecializationAxiom(ax)
    acc.copy(conceptSpecializationAxioms = acc.conceptSpecializationAxioms :+
      schema.tables.ConceptSpecializationAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        subConceptUUID = ops.getTermUUID(info.sub).toString,
        superConceptUUID = ops.getTermUUID(info.sup).toString))
  }

  def funReifiedRelationshipSpecializationAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ReifiedRelationshipSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromReifiedRelationshipSpecializationAxiom(ax)
    acc.copy(reifiedRelationshipSpecializationAxioms = acc.reifiedRelationshipSpecializationAxioms :+
      schema.tables.ReifiedRelationshipSpecializationAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        subRelationshipUUID = ops.getTermUUID(info.sub).toString,
        superRelationshipUUID = ops.getTermUUID(info.sup).toString))
  }

  def funEntityExistentialRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityExistentialRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityRestrictionAxiom(ax)
    acc.copy(entityExistentialRestrictionAxioms = acc.entityExistentialRestrictionAxioms :+
      schema.tables.EntityExistentialRestrictionAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        restrictedDomainUUID = ops.getTermUUID(info.domain).toString,
        restrictedRangeUUID = ops.getTermUUID(info.range).toString,
        restrictedRelationUUID = ops.getTermUUID(info.restrictedRelation).toString))
  }

  def funEntityUniversalRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityUniversalRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityRestrictionAxiom(ax)
    acc.copy(entityUniversalRestrictionAxioms = acc.entityUniversalRestrictionAxioms :+
      schema.tables.EntityUniversalRestrictionAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        restrictedDomainUUID = ops.getTermUUID(info.domain).toString,
        restrictedRangeUUID = ops.getTermUUID(info.range).toString,
        restrictedRelationUUID = ops.getTermUUID(info.restrictedRelation).toString))
  }

  def funEntityScalarDataPropertyExistentialRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyExistentialRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyExistentialRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyExistentialRestrictionAxioms = acc.entityScalarDataPropertyExistentialRestrictionAxioms :+
      schema.tables.EntityScalarDataPropertyExistentialRestrictionAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        restrictedEntityUUID = ops.getTermUUID(info.restrictedEntity).toString,
        scalarPropertyUUID = ops.getTermUUID(info.scalarDataProperty).toString,
        scalarRestrictionUUID = ops.getTermUUID(info.restrictedRange).toString))
  }

  def funEntityScalarDataPropertyParticularRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyParticularRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyParticularRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyParticularRestrictionAxioms = acc.entityScalarDataPropertyParticularRestrictionAxioms :+
      schema.tables.EntityScalarDataPropertyParticularRestrictionAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        restrictedEntityUUID = ops.getTermUUID(info.restrictedEntity).toString,
        scalarPropertyUUID = ops.getTermUUID(info.scalarDataProperty).toString,
        literalValue = info.literalValue))
  }

  def funEntityScalarDataPropertyUniversalRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyUniversalRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyUniversalRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyUniversalRestrictionAxioms = acc.entityScalarDataPropertyUniversalRestrictionAxioms :+
      schema.tables.EntityScalarDataPropertyUniversalRestrictionAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        restrictedEntityUUID = ops.getTermUUID(info.restrictedEntity).toString,
        scalarPropertyUUID = ops.getTermUUID(info.scalarDataProperty).toString,
        scalarRestrictionUUID = ops.getTermUUID(info.restrictedRange).toString))
  }

  def funScalarOneOfLiteralAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ScalarOneOfLiteralAxiom)
  : Axioms
  = {
    val info = ops.fromScalarOneOfLiteralAxiom(ax)
    acc.copy(scalarOneOfLiteralAxioms = acc.scalarOneOfLiteralAxioms :+
      schema.tables.ScalarOneOfLiteralAxiom(
        graphUUID = guuid,
        uuid = info.uuid.toString,
        axiomUUID = ops.getTermUUID(info.restriction).toString,
        value = info.value))
  }

  def combine[omf <: OMF]
  (guuid: String, ops: OMFOps[omf])
  (acc: Axioms,
   ax: omf#Axiom)
  : Axioms
  = ops.foldAxiom[Axioms](
    funAspectSpecializationAxiom =
      Axioms.funAspectSpecializationAxiom(guuid, ops, acc),
    funConceptSpecializationAxiom =
      Axioms.funConceptSpecializationAxiom(guuid, ops, acc),
    funReifiedRelationshipSpecializationAxiom =
      Axioms.funReifiedRelationshipSpecializationAxiom(guuid, ops, acc),
    funEntityExistentialRestrictionAxiom =
      Axioms.funEntityExistentialRestrictionAxiom(guuid, ops, acc),
    funEntityUniversalRestrictionAxiom =
      Axioms.funEntityUniversalRestrictionAxiom(guuid, ops, acc),
    funEntityScalarDataPropertyExistentialRestrictionAxiom =
      Axioms.funEntityScalarDataPropertyExistentialRestrictionAxiom(guuid, ops, acc),
    funEntityScalarDataPropertyParticularRestrictionAxiom =
      Axioms.funEntityScalarDataPropertyParticularRestrictionAxiom(guuid, ops, acc),
    funEntityScalarDataPropertyUniversalRestrictionAxiom =
      Axioms.funEntityScalarDataPropertyUniversalRestrictionAxiom(guuid, ops, acc),
    funScalarOneOfLiteralAxiom =
      Axioms.funScalarOneOfLiteralAxiom(guuid, ops, acc)
  )(ax)
}

object OMFTabularExport {

  // @see https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/enum.scala

  import scala.Ordering

  import shapeless.{ Generic, ::, HList, HNil }
  // Derive an Ordering for an HList from the Orderings of its elements

  trait LowPriorityGenericOrdering {
    // An Ordering for any type which is isomorphic to an HList, if that HList has an Ordering

    implicit def hlistIsoOrdering[A, H <: HList]
    (implicit gen : Generic.Aux[A, H], oh : Ordering[H])
    : Ordering[A]
    = new Ordering[A] {
      def compare(a1 : A, a2 : A) = oh.compare(gen to a1, gen to a2)
    }
  }

  object GenericOrdering extends LowPriorityGenericOrdering {
    implicit def hnilOrdering : Ordering[HNil] = new Ordering[HNil] {
      def compare(a : HNil, b : HNil) = 0
    }

    implicit def hlistOrdering[H, T <: HList]
    (implicit oh : Ordering[H], ot : Ordering[T])
    : Ordering[H :: T]
    = new Ordering[H :: T] {
      def compare(a : H :: T, b : H :: T) = {
        val i = oh.compare(a.head, b.head)
        if (i == 0) ot.compare(a.tail, b.tail)
        else i
      }
    }
  }

  import GenericOrdering._

  implicit def TerminologyGraphKindOrdering: Ordering[schema.tables.TerminologyGraphKind] =
  new Ordering[schema.tables.TerminologyGraphKind] {
    def compare(x: schema.tables.TerminologyGraphKind, y: schema.tables.TerminologyGraphKind): scala.Int
    = if (x == y) 0
    else if (x == schema.tables.OpenWorldDefinitions) -1
    else 1
  }

  def toTables[omf <: OMF]
  (s: Set[omf#ImmutableTerminologyBox])
  (implicit store: omf#Store, ops: OMFOps[omf])
  : schema.tables.OMFSchemaTables
  = {
    val as
    : SortedSet[Annotation]
    = s.foldLeft[SortedSet[Annotation]](SortedSet.empty[Annotation])(_ ++ ops.getAnnotations(_))

    val bs: Set[omf#ImmutableBundle] = s.flatMap(ops.foldImmutableBundle)

    val tsigs = s.map(g => g -> ops.fromTerminology(g))

    val allTGraphs = tsigs.flatMap {
      case (_, sig) if !sig.isBundle =>
        Some(schema.tables.TerminologyGraph(
          uuid = sig.uuid.toString,
          kind = if (TerminologyKind.isDefinitionKind(sig.kind))
            schema.tables.OpenWorldDefinitions
          else
            schema.tables.ClosedWorldDesignations,
          name = sig.name,
          iri = sig.iri.toString))
      case _ =>
        None
    }.to[Seq].sorted

    val allBGraphs = tsigs.flatMap {
      case (_, sig) if sig.isBundle =>
        Some(schema.tables.Bundle(
          uuid = sig.uuid.toString,
          kind = if (TerminologyKind.isDefinitionKind(sig.kind))
            schema.tables.OpenWorldDefinitions
          else
            schema.tables.ClosedWorldDesignations,
          name = sig.name,
          iri = sig.iri.toString))
      case _ =>
        None
    }.to[Seq].sorted

    val allBundledTerminologyAxioms = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.bAxioms.map { gax =>
        val info = ops.fromBundledTerminologyAxiom(gax)
        schema.tables.BundledTerminologyAxiom(
          uuid = info.uuid.toString,
          terminologyBundleUUID = ops.getTerminologyUUID(info.bundle).toString,
          bundledTerminologyUUID = ops.getTerminologyUUID(info.bundledTerminology).toString
        )
      }
    }.to[Seq].sorted

    val allAnonymousConceptTaxonomyAxioms = tsigs.flatMap { case (b, sig) =>
      val guuid = sig.uuid.toString
      sig.aTAxioms.map { ax =>
        val info = ops.fromAnonymousConceptTaxonomyAxiom(ax)
        schema.tables.AnonymousConceptTaxonomyAxiom(
          uuid = info.uuid.toString,
          bundleUUID = guuid,
          disjointTaxonomyParentUUID = ops.getConceptTreeDisjunctionUUID(info.disjointTaxonomyParent).toString
        )
      }
    }.to[Seq].sorted

    val allRootConceptTaxonomyAxioms = tsigs.flatMap { case (b, sig) =>
      val guuid = sig.uuid.toString
      sig.rTAxioms.map { ax =>
        val info = ops.fromRootConceptTaxonomyAxiom(ax)
        schema.tables.RootConceptTaxonomyAxiom(
          uuid = info.uuid.toString,
          bundleUUID = guuid,
          rootUUID = ops.getTermUUID(info.root).toString
        )
      }
    }.to[Seq].sorted

    val allSpecificDisjointConceptAxioms = tsigs.flatMap { case (b, sig) =>
      val guuid = sig.uuid.toString
      sig.sTAxioms.map { ax =>
        val info = ops.fromSpecificDisjointConceptAxiom(ax)
        schema.tables.SpecificDisjointConceptAxiom(
          uuid = info.uuid.toString,
          bundleUUID = guuid,
          disjointLeafUUID = ops.getTermUUID(info.disjointLeaf).toString,
          disjointTaxonomyParentUUID = ops.getConceptTreeDisjunctionUUID(info.disjointTaxonomyParent).toString
        )
      }
    }.to[Seq].sorted

    val allConceptDesignationAxioms = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.gaxioms.flatMap { gax =>
        ops.foldTerminologyBoxAxiom[Option[schema.tables.ConceptDesignationTerminologyAxiom]](
          funConceptDesignationTerminologyAxiom =
            (gax: omf#ConceptDesignationTerminologyAxiom) => {
              val info = ops.fromConceptDesignationTerminologyAxiom(gax)
              Some(schema.tables.ConceptDesignationTerminologyAxiom(
                uuid = ops.getTerminologyAxiomUUID(gax).toString,
                designatedConceptUUID = ops.getTermUUID(info.parentConcept).toString,
                designationTerminologyGraphUUID = ops.getTerminologyUUID(info.parentGraph).toString))
            },
          funTerminologyGraphDirectExtensionAxiom =
            (_: omf#TerminologyExtensionAxiom) =>
              None,
          funTerminologyGraphDirectNestingAxiom =
            (_: omf#TerminologyNestingAxiom) =>
              None)(gax)
      }
    }.to[Seq].sorted

    val allExtensionAxioms = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.gaxioms.flatMap { gax =>
        ops.foldTerminologyBoxAxiom[Option[schema.tables.TerminologyExtensionAxiom]](
          funConceptDesignationTerminologyAxiom =
            (_: omf#ConceptDesignationTerminologyAxiom) =>
              None,
          funTerminologyGraphDirectExtensionAxiom =
            (gax: omf#TerminologyExtensionAxiom) => {
              val info = ops.fromTerminologyExtensionAxiom(gax)
              Some(
                schema.tables.TerminologyExtensionAxiom(
                  uuid = info.uuid.toString,
                  extendedTerminologyUUID = ops.getTerminologyUUID(info.extendedTerminology).toString,
                  extendingTerminologyUUID = guuid)
              )
            },
          funTerminologyGraphDirectNestingAxiom =
            (_: omf#TerminologyNestingAxiom) =>
              None
        )(gax)
      }
    }.to[Seq].sorted

    val allNestingAxioms = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.gaxioms.flatMap { gax =>
        ops.foldTerminologyBoxAxiom[Option[schema.tables.TerminologyNestingAxiom]](
          funConceptDesignationTerminologyAxiom =
            (_: omf#ConceptDesignationTerminologyAxiom) =>
              None,
          funTerminologyGraphDirectExtensionAxiom =
            (_: omf#TerminologyExtensionAxiom) =>
              None,
          funTerminologyGraphDirectNestingAxiom =
            (gax: omf#TerminologyNestingAxiom) => {
              val info = ops.fromTerminologyNestingAxiom(gax)
              Some(
                schema.tables.TerminologyNestingAxiom(
                  uuid = info.uuid.toString,
                  nestedTerminologyUUID = guuid,
                  nestingContextUUID = ops.getTermUUID(info.nestingContext).toString,
                  nestingTerminologyUUID = ops.getTerminologyUUID(info.nestingTerminology).toString)
              )
            }
        )(gax)
      }
    }.to[Seq].sorted

    val allAspects = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.aspects.map { a =>
        schema.tables.Aspect(
          graphUUID = guuid,
          uuid = ops.getTermUUID(a).toString,
          name = ops.getTermName(a),
          iri = ops.getTermIRI(a).toString)
      }
    }.to[Seq].sorted

    val allConcepts = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.concepts.map { c =>
        val sig = ops.fromConcept(c)
        schema.tables.Concept(
          graphUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name,
          iri = sig.iri.toString,
          isAbstract = sig.isAbstract)
      }
    }.to[Seq].sorted

    val allReifiedRelationships = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.reifiedRelationships.map { rr =>
        val sig = ops.fromReifiedRelationship(rr)
        schema.tables.ReifiedRelationship(
          graphUUID = guuid,
          uuid = sig.uuid.toString,
          isAbstract = sig.isAbstract,
          name = sig.name,
          unreifiedPropertyName = sig.unreifiedPropertyName,
          unreifiedInversePropertyName = sig.unreifiedInversePropertyName,
          iri = sig.iri.toString,
          isAsymmetric = sig.characteristics.exists(RelationshipCharacteristics.isAsymmetric == _),
          isEssential = sig.characteristics.exists(RelationshipCharacteristics.isEssential == _),
          isFunctional = sig.characteristics.exists(RelationshipCharacteristics.isFunctional == _),
          isInverseEssential = sig.characteristics.exists(RelationshipCharacteristics.isInverseEssential == _),
          isInverseFunctional = sig.characteristics.exists(RelationshipCharacteristics.isInverseFunctional == _),
          isIrreflexive = sig.characteristics.exists(RelationshipCharacteristics.isIrreflexive == _),
          isReflexive = sig.characteristics.exists(RelationshipCharacteristics.isReflexive == _),
          isSymmetric = sig.characteristics.exists(RelationshipCharacteristics.isSymmetric == _),
          isTransitive = sig.characteristics.exists(RelationshipCharacteristics.isTransitive == _),
          sourceUUID = ops.getTermUUID(sig.source).toString,
          targetUUID = ops.getTermUUID(sig.target).toString)
      }
    }.to[Seq].sorted

    val allUnreifiedRelationships = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.unreifiedRelationships.map { ur =>
        val sig = ops.fromUnreifiedRelationship(ur)
        schema.tables.UnreifiedRelationship(
          graphUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name,
          iri = sig.iri.toString,
          isAsymmetric = sig.characteristics.exists(RelationshipCharacteristics.isAsymmetric == _),
          isEssential = sig.characteristics.exists(RelationshipCharacteristics.isEssential == _),
          isFunctional = sig.characteristics.exists(RelationshipCharacteristics.isFunctional == _),
          isInverseEssential = sig.characteristics.exists(RelationshipCharacteristics.isInverseEssential == _),
          isInverseFunctional = sig.characteristics.exists(RelationshipCharacteristics.isInverseFunctional == _),
          isIrreflexive = sig.characteristics.exists(RelationshipCharacteristics.isIrreflexive == _),
          isReflexive = sig.characteristics.exists(RelationshipCharacteristics.isReflexive == _),
          isSymmetric = sig.characteristics.exists(RelationshipCharacteristics.isSymmetric == _),
          isTransitive = sig.characteristics.exists(RelationshipCharacteristics.isTransitive == _),
          sourceUUID = ops.getTermUUID(sig.source).toString,
          targetUUID = ops.getTermUUID(sig.target).toString)
      }
    }.to[Seq].sorted

    val allScalars = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.scalarDataTypes.map { sc =>
        schema.tables.Scalar(
          graphUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermName(sc),
          iri = ops.getTermIRI(sc).toString)
      }
    }.to[Seq].sorted

    val allStructures = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structuredDataTypes.map { sc =>
        schema.tables.Structure(
          graphUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermName(sc),
          iri = ops.getTermIRI(sc).toString)
      }
    }.to[Seq].sorted

    val allEntity2ScalarProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entityScalarDataProperties.map { e2sc =>
        val info = ops.fromEntityScalarDataProperty(e2sc)
        schema.tables.EntityScalarDataProperty(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted
    
    val allEntity2StructureProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entityStructuredDataProperties.map { e2sc =>
        val info = ops.fromEntityStructuredDataProperty(e2sc)
        schema.tables.EntityStructuredDataProperty(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted

    val allScalarProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.scalarDataProperties.map { s2sc =>
        val info = ops.fromScalarDataProperty(s2sc)
        schema.tables.ScalarDataProperty(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted

    val allStructuredProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structuredDataProperties.map { s2sc =>
        val info = ops.fromStructuredDataProperty(s2sc)
        schema.tables.StructuredDataProperty(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted

    val allBinaryScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.binaryScalarRestrictions.map { r =>
        val info = ops.fromBinaryScalarRestriction(r)
        schema.tables.BinaryScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allIRIScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.iriScalarRestrictions.map { r =>
        val info = ops.fromIRIScalarRestriction(r)
        schema.tables.IRIScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allNumericScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.numericScalarRestrictions.map { r =>
        val info = ops.fromNumericScalarRestriction(r)
        schema.tables.NumericScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          maxExclusive = info.maxExclusive,
          maxInclusive = info.maxInclusive,
          minExclusive = info.minExclusive,
          minInclusive = info.minInclusive,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allPlainLiteralScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.plainLiteralScalarRestrictions.map { r =>
        val info = ops.fromPlainLiteralScalarRestriction(r)
        schema.tables.PlainLiteralScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          language = info.language,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allScalarOneOfRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.scalarOneOfRestrictions.map { r =>
        val info = ops.fromScalarOneOfRestriction(r)
        schema.tables.ScalarOneOfRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allStringScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.stringScalarRestrictions.map { r =>
        val info = ops.fromStringScalarRestriction(r)
        schema.tables.StringScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allTimeScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.timeScalarRestrictions.map { r =>
        val info = ops.fromTimeScalarRestriction(r)
        schema.tables.TimeScalarRestriction(
          graphUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          iri = info.iri.toString,
          maxExclusive = info.maxExclusive,
          maxInclusive = info.maxInclusive,
          minExclusive = info.minExclusive,
          minInclusive = info.minInclusive,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allAxioms = tsigs.aggregate(Axioms())(
      seqop = {
        case (acc: Axioms, (g, sig)) =>
          val guuid = sig.uuid.toString
          val axioms = sig.axioms.aggregate(Axioms())(seqop=Axioms.combine(guuid, ops),combop=Axioms.append)
          Axioms.append(acc, axioms)
      },
      combop = Axioms.append)

    val t = schema.tables.OMFSchemaTables.createEmptyOMFSchemaTables().copy(
      annotationProperties = ops.annotationProperties(),
      // graphs
      terminologyGraphs =
        allTGraphs,
      bundles =
        allBGraphs,

      // graph axioms
      conceptDesignationTerminologyAxioms =
        allConceptDesignationAxioms,
      terminologyExtensionAxioms =
        allExtensionAxioms,
      terminologyNestingAxioms =
        allNestingAxioms,

      // terms
      aspects =
        allAspects,
      concepts =
        allConcepts,
      reifiedRelationships =
        allReifiedRelationships,
      unreifiedRelationships =
        allUnreifiedRelationships,
      scalars =
        allScalars,
      structures =
        allStructures,

      binaryScalarRestrictions =
        allBinaryScalarRestrictions,
      iriScalarRestrictions =
        allIRIScalarRestrictions,
      numericScalarRestrictions =
        allNumericScalarRestrictions,
      plainLiteralScalarRestrictions =
        allPlainLiteralScalarRestrictions,
      scalarOneOfRestrictions =
        allScalarOneOfRestrictions,
      stringScalarRestrictions =
        allStringScalarRestrictions,
      timeScalarRestrictions =
        allTimeScalarRestrictions,

      entityScalarDataProperties =
        allEntity2ScalarProperties,
      entityStructuredDataProperties =
        allEntity2StructureProperties,
      scalarDataProperties =
        allScalarProperties,
      structuredDataProperties =
        allStructuredProperties,

      // axioms
      aspectSpecializationAxioms =
        allAxioms.aspectSpecializationAxioms.sorted,
      conceptSpecializationAxioms =
        allAxioms.conceptSpecializationAxioms.sorted,
      reifiedRelationshipSpecializationAxioms =
        allAxioms.reifiedRelationshipSpecializationAxioms.sorted,
      entityExistentialRestrictionAxioms =
        allAxioms.entityExistentialRestrictionAxioms.sorted,
      entityUniversalRestrictionAxioms =
        allAxioms.entityUniversalRestrictionAxioms.sorted,
      entityScalarDataPropertyExistentialRestrictionAxioms =
        allAxioms.entityScalarDataPropertyExistentialRestrictionAxioms.sorted,
      entityScalarDataPropertyParticularRestrictionAxioms =
        allAxioms.entityScalarDataPropertyParticularRestrictionAxioms.sorted,
      entityScalarDataPropertyUniversalRestrictionAxioms =
        allAxioms.entityScalarDataPropertyUniversalRestrictionAxioms.sorted,
      scalarOneOfLiteralAxioms =
        allAxioms.scalarOneOfLiteralAxioms.sorted,

      bundledTerminologyAxioms =
        allBundledTerminologyAxioms,

      anonymousConceptTaxonomyAxioms =
        allAnonymousConceptTaxonomyAxioms,
      rootConceptTaxonomyAxioms =
        allRootConceptTaxonomyAxioms,
      specificDisjointConceptAxioms =
        allSpecificDisjointConceptAxioms,

      annotations =
        as.iterator.to[Seq]
    )

    t
  }

}
