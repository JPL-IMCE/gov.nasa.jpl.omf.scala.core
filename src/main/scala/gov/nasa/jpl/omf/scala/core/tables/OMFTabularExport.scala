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

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.imce._
import gov.nasa.jpl.imce.oml.resolver.OMLTablesResolver

import scala.collection.immutable.{Map, Seq, Set}
import scala.{None, Option, Some}
import scala.Predef.{ArrowAssoc, String}

case class Axioms
( aspectSpecializationAxioms : Seq[oml.tables.AspectSpecializationAxiom] = Seq.empty,
  conceptSpecializationAxioms : Seq[oml.tables.ConceptSpecializationAxiom] = Seq.empty,
  reifiedRelationshipSpecializationAxioms : Seq[oml.tables.ReifiedRelationshipSpecializationAxiom] = Seq.empty,

  entityExistentialRestrictionAxioms : Seq[oml.tables.EntityExistentialRestrictionAxiom] = Seq.empty,
  entityUniversalRestrictionAxioms : Seq[oml.tables.EntityUniversalRestrictionAxiom] = Seq.empty,

  entityScalarDataPropertyExistentialRestrictionAxioms : Seq[oml.tables.EntityScalarDataPropertyExistentialRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyParticularRestrictionAxioms : Seq[oml.tables.EntityScalarDataPropertyParticularRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyUniversalRestrictionAxioms : Seq[oml.tables.EntityScalarDataPropertyUniversalRestrictionAxiom] = Seq.empty,

  scalarOneOfLiteralAxioms : Seq[oml.tables.ScalarOneOfLiteralAxiom] = Seq.empty )

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
      oml.tables.AspectSpecializationAxiom(
        uuid = info.uuid.toString,
        tboxUUID = guuid,
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
      oml.tables.ConceptSpecializationAxiom(
        uuid = info.uuid.toString,
        tboxUUID = guuid,
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
      oml.tables.ReifiedRelationshipSpecializationAxiom(
        tboxUUID = guuid,
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
      oml.tables.EntityExistentialRestrictionAxiom(
        tboxUUID = guuid,
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
      oml.tables.EntityUniversalRestrictionAxiom(
        tboxUUID = guuid,
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
      oml.tables.EntityScalarDataPropertyExistentialRestrictionAxiom(
        tboxUUID = guuid,
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
      oml.tables.EntityScalarDataPropertyParticularRestrictionAxiom(
        tboxUUID = guuid,
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
      oml.tables.EntityScalarDataPropertyUniversalRestrictionAxiom(
        tboxUUID = guuid,
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
      oml.tables.ScalarOneOfLiteralAxiom(
        tboxUUID = guuid,
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

  implicit def TerminologyKindOrdering: Ordering[oml.tables.TerminologyKind] =
  new Ordering[oml.tables.TerminologyKind] {
    def compare(x: oml.tables.TerminologyKind, y: oml.tables.TerminologyKind): scala.Int
    = if (x == y) 0
    else if (x == oml.tables.OpenWorldDefinitions) -1
    else 1
  }

  def toTables[omf <: OMF]
  (s: Set[omf#ImmutableTerminologyBox])
  (implicit store: omf#Store, ops: OMFOps[omf])
  : oml.tables.OMLSpecificationTables
  = {
    val as
    : Map[oml.tables.AnnotationProperty, Seq[oml.tables.AnnotationEntry]]
    = s
      .foldLeft[Map[oml.tables.AnnotationProperty, Seq[oml.tables.AnnotationEntry]]](Map.empty) { case (acc, tbox) =>
      val next = OMLTablesResolver.mergeMapOfSeq(acc, ops.getAnnotations(tbox))
      next
    }

    val bs: Set[omf#ImmutableBundle] = s.flatMap(ops.foldImmutableBundle)

    val tsigs = s.map(g => g -> ops.fromTerminology(g))

    val allTGraphs = tsigs.flatMap {
      case (_, sig) if !sig.isBundle =>
        Some(oml.tables.TerminologyGraph(
          uuid = sig.uuid.toString,
          kind = if (TerminologyKind.isDefinitionKind(sig.kind))
            oml.tables.OpenWorldDefinitions
          else
            oml.tables.ClosedWorldDesignations,
          iri = sig.iri.toString))
      case _ =>
        None
    }.to[Seq].sorted

    val allBGraphs = tsigs.flatMap {
      case (_, sig) if sig.isBundle =>
        Some(oml.tables.Bundle(
          uuid = sig.uuid.toString,
          kind = if (TerminologyKind.isDefinitionKind(sig.kind))
            oml.tables.OpenWorldDefinitions
          else
            oml.tables.ClosedWorldDesignations,
          iri = sig.iri.toString))
      case _ =>
        None
    }.to[Seq].sorted

    val allBundledTerminologyAxioms = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.bAxioms.map { gax =>
        val info = ops.fromBundledTerminologyAxiom(gax)
        oml.tables.BundledTerminologyAxiom(
          uuid = info.uuid.toString,
          bundledTerminologyUUID = ops.getTerminologyUUID(info.bundledTerminology).toString,
          bundleUUID = ops.getTerminologyUUID(info.bundle).toString
        )
      }
    }.to[Seq].sorted

    val allAnonymousConceptTaxonomyAxioms = tsigs.flatMap { case (b, sig) =>
      val guuid = sig.uuid.toString
      sig.aTAxioms.map { ax =>
        val info = ops.fromAnonymousConceptTaxonomyAxiom(ax)
        oml.tables.AnonymousConceptTaxonomyAxiom(
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
        oml.tables.RootConceptTaxonomyAxiom(
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
        oml.tables.SpecificDisjointConceptAxiom(
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
        ops.foldTerminologyBoxAxiom[Option[oml.tables.ConceptDesignationTerminologyAxiom]](
          funConceptDesignationTerminologyAxiom =
            (gax: omf#ConceptDesignationTerminologyAxiom) => {
              val info = ops.fromConceptDesignationTerminologyAxiom(gax)
              Some(oml.tables.ConceptDesignationTerminologyAxiom(
                uuid = ops.getTerminologyAxiomUUID(gax).toString,
                tboxUUID = ops.getTerminologyUUID(info.parentGraph).toString,
                designatedConceptUUID = ops.getTermUUID(info.parentConcept).toString))
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
        ops.foldTerminologyBoxAxiom[Option[oml.tables.TerminologyExtensionAxiom]](
          funConceptDesignationTerminologyAxiom =
            (_: omf#ConceptDesignationTerminologyAxiom) =>
              None,
          funTerminologyGraphDirectExtensionAxiom =
            (gax: omf#TerminologyExtensionAxiom) => {
              val info = ops.fromTerminologyExtensionAxiom(gax)
              Some(
                oml.tables.TerminologyExtensionAxiom(
                  uuid = info.uuid.toString,
                  tboxUUID = guuid,
                  extendedTerminologyUUID = ops.getTerminologyUUID(info.extendedTerminology).toString)
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
        ops.foldTerminologyBoxAxiom[Option[oml.tables.TerminologyNestingAxiom]](
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
                oml.tables.TerminologyNestingAxiom(
                  uuid = info.uuid.toString,
                  tboxUUID = guuid,
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
        oml.tables.Aspect(
          tboxUUID = guuid,
          uuid = ops.getTermUUID(a).toString,
          name = ops.getTermName(a))
      }
    }.to[Seq].sorted

    val allConcepts = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.concepts.map { c =>
        val sig = ops.fromConcept(c)
        oml.tables.Concept(
          tboxUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name)
      }
    }.to[Seq].sorted

    val allReifiedRelationships = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.reifiedRelationships.map { rr =>
        val sig = ops.fromReifiedRelationship(rr)
        oml.tables.ReifiedRelationship(
          tboxUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name,
          unreifiedPropertyName = sig.unreifiedPropertyName,
          unreifiedInversePropertyName = sig.unreifiedInversePropertyName,
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
        oml.tables.UnreifiedRelationship(
          tboxUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name,
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
        oml.tables.Scalar(
          tboxUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermName(sc))
      }
    }.to[Seq].sorted

    val allStructures = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structuredDataTypes.map { sc =>
        oml.tables.Structure(
          tboxUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermName(sc))
      }
    }.to[Seq].sorted

    val allEntity2ScalarProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entityScalarDataProperties.map { e2sc =>
        val info = ops.fromEntityScalarDataProperty(e2sc)
        oml.tables.EntityScalarDataProperty(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString,
          isIdentityCriteria = info.isIdentityCriteria,
          name = info.name)
      }
    }.to[Seq].sorted
    
    val allEntity2StructureProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entityStructuredDataProperties.map { e2sc =>
        val info = ops.fromEntityStructuredDataProperty(e2sc)
        oml.tables.EntityStructuredDataProperty(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString,
          isIdentityCriteria = info.isIdentityCriteria,
          name = info.name)
      }
    }.to[Seq].sorted

    val allScalarProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.scalarDataProperties.map { s2sc =>
        val info = ops.fromScalarDataProperty(s2sc)
        oml.tables.ScalarDataProperty(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted

    val allStructuredProperties = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structuredDataProperties.map { s2sc =>
        val info = ops.fromStructuredDataProperty(s2sc)
        oml.tables.StructuredDataProperty(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          domainUUID = ops.getTermUUID(info.domain).toString,
          rangeUUID = ops.getTermUUID(info.range).toString)
      }
    }.to[Seq].sorted

    val allBinaryScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.binaryScalarRestrictions.map { r =>
        val info = ops.fromBinaryScalarRestriction(r)
        oml.tables.BinaryScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
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
        oml.tables.IRIScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
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
        oml.tables.NumericScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
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
        oml.tables.PlainLiteralScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          langRange = info.langRange,
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
        oml.tables.ScalarOneOfRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
          restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
      }
    }.to[Seq].sorted

    val allStringScalarRestrictions = tsigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.stringScalarRestrictions.map { r =>
        val info = ops.fromStringScalarRestriction(r)
        oml.tables.StringScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
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
        oml.tables.TimeScalarRestriction(
          tboxUUID = guuid,
          uuid = info.uuid.toString,
          name = info.name,
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

    val t = oml.tables.OMLSpecificationTables.createEmptyOMLSpecificationTables().copy(
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
        as
    )

    t
  }

}
