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

import java.lang.System

import gov.nasa.jpl.imce.oml
import gov.nasa.jpl.imce.oml.parallelSort
import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString
import gov.nasa.jpl.imce.oml.tables.taggedTypes
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{OMF, OMFError, OMFOps, RelationshipCharacteristics, TerminologyKind}

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMFTabularExportFromBundle {

  implicit def toIRI[omf <: OMF[omf]](iri: omf#IRI): taggedTypes.IRI = taggedTypes.iri(iri.toString)

  def toTables[omf <: OMF[omf]]
  (acc: Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)])
  (bundle: omf#ImmutableBundle)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)]
  = for {
    im2st <- acc
    all_tboxes = im2st.map(_._1).toSet[omf#Module]
    all_tables = im2st.map(_._2).to[Set]

    all_aps = all_tables.flatMap(_.annotationProperties)

    s = ops.fromImmutableTerminology(bundle)
    suuid = s.uuid.asInstanceOf[resolver.api.taggedTypes.BundleUUID]

    oug = oml.uuid.JVMUUIDGenerator()

    s_common_aps = s.annotationProperties intersect all_aps
    allAnnotationProperties = parallelSort.parSortBy(
      (s.annotationProperties -- s_common_aps).to[Seq],
      (x: oml.tables.AnnotationProperty) => x.uuid)(taggedTypes.orderingAnnotationPropertyUUID)

    // Check that there are no overlaping annotation properties
    _ = {
      if (s_common_aps.nonEmpty) {
        val common = parallelSort.parSortBy(s_common_aps.to[Seq], (ap: oml.tables.AnnotationProperty) => ap.abbrevIRI.toString)
        System.out.println(
          s"Bundle ${s.iri} duplicates ${common.size} Annotations defined in imported modules: " +
            common.map(_.abbrevIRI).mkString("\n\t", ", ", "\n"))
      }
    }

    conceptDesignationTerminologyAxioms <-
      s.conceptDesignation.foldLeft(
        Seq.empty[oml.tables.ConceptDesignationTerminologyAxiom].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromConceptDesignationTerminologyAxiom(omf_ax)
          _ <- if (all_tboxes.exists { m => ops.getModuleIRI(m) == omf_info.designatedTerminology })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"Bundle ${s.iri} has a ConceptDesignationTerminologyAxiom (uuid=${omf_info.uuid}) " +
                s" whose designated terminology is not imported: ${omf_info.designatedTerminology}"))
              .left[Unit]
          ax = oml.tables.ConceptDesignationTerminologyAxiom(
            uuid = omf_info.uuid,
            tboxUUID = omf_info.graphUUID,
            designatedConceptUUID = ops.getConceptKindUUID(omf_info.designatedConcept),
            designatedTerminologyIRI = omf_info.designatedTerminology)

        } yield axs :+ ax
      }

    allConceptDesignationTerminologyAxioms = parallelSort.parSortBy(
      conceptDesignationTerminologyAxioms,
      (x: oml.tables.ConceptDesignationTerminologyAxiom) => x.uuid)(taggedTypes.orderingConceptDesignationTerminologyAxiomUUID)

    extensionAxioms <-
      s.extensions.foldLeft(
        Seq.empty[oml.tables.TerminologyExtensionAxiom].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromTerminologyExtensionAxiom(omf_ax)
          _ <- if (all_tboxes.exists { m => ops.getModuleIRI(m) == omf_info.extendedTerminology })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"Bundle ${s.iri} has a TerminologyExtensionAxiom (uuid=${omf_info.uuid}) " +
                s" whose extended terminology is not imported: ${omf_info.extendedTerminology}"))
              .left[Unit]
          ax = oml.tables.TerminologyExtensionAxiom(
            uuid = omf_info.uuid,
            tboxUUID = suuid,
            extendedTerminologyIRI = omf_info.extendedTerminology)

        } yield axs :+ ax
      }

    allExtensionAxioms = parallelSort.parSortBy(
      extensionAxioms,
      (x: oml.tables.TerminologyExtensionAxiom) => x.uuid)(taggedTypes.orderingTerminologyExtensionAxiomUUID)

    nestingAxioms <-
      s.nesting.foldLeft(
        Seq.empty[oml.tables.TerminologyNestingAxiom].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromTerminologyNestingAxiom(omf_ax)
          _ <- if (all_tboxes.exists { m => ops.getModuleIRI(m) == omf_info.nestingTerminology })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"Bundle ${s.iri} has a TerminologyNestingAxiom (uuid=${omf_info.uuid}) " +
                s" whose nesting terminology is not imported: ${omf_info.nestingTerminology}"))
              .left[Unit]
          ax = oml.tables.TerminologyNestingAxiom(
            uuid = omf_info.uuid,
            tboxUUID = suuid,
            nestingTerminologyIRI = omf_info.nestingTerminology,
            nestingContextUUID = ops.getConceptKindUUID(omf_info.nestingContext))

        } yield axs :+ ax
      }

    allNestingAxioms = parallelSort.parSortBy(
      nestingAxioms,
      (x: oml.tables.TerminologyNestingAxiom) => x.uuid)(taggedTypes.orderingTerminologyNestingAxiomUUID)

    bundledTerminologyAxioms <-
      s.bundledTerminologies.foldLeft(
        Seq.empty[oml.tables.BundledTerminologyAxiom].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromBundledTerminologyAxiom(omf_ax)
          _ <- if (all_tboxes.exists { m => ops.getModuleIRI(m) == omf_info.bundledTerminology })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"Bundle ${s.iri} has a BundledTerminologyAxiom (uuid=${omf_info.uuid}) " +
                s" whose bundled terminology is not imported: ${omf_info.bundledTerminology}"))
              .left[Unit]
          ax = oml.tables.BundledTerminologyAxiom(
            uuid = omf_info.uuid,
            bundleUUID = suuid,
            bundledTerminologyIRI = omf_info.bundledTerminology)

        } yield axs :+ ax
      }

    allBundledTerminologyAxioms = parallelSort.parSortBy(
      bundledTerminologyAxioms,
      (x: oml.tables.BundledTerminologyAxiom) => x.uuid)(taggedTypes.orderingBundledTerminologyAxiomUUID)

    allAspects = parallelSort.parSortBy(
      s.aspects.map { a =>
        oml.tables.Aspect(
          tboxUUID = suuid,
          uuid = ops.getAspectUUID(a),
          name = ops.getTermName(a))
      }.to[Seq],
      (a: oml.tables.Aspect) => a.uuid)(taggedTypes.orderingAspectUUID)

    allConcepts = parallelSort.parSortBy(
      s.concepts.map { c =>
        oml.tables.Concept(
          tboxUUID = suuid,
          uuid = ops.getConceptUUID(c),
          name = ops.getTermName(c))
      }.to[Seq],
      (c: oml.tables.Concept) => c.uuid)(taggedTypes.orderingConceptUUID)

    allReifiedRelationshipRestrictions = parallelSort.parSortBy(
      s.reifiedRelationshipRestrictions.map { rr =>
        val sig = ops.fromReifiedRelationshipRestriction(rr)
        oml.tables.ReifiedRelationshipRestriction(
          tboxUUID = suuid,
          uuid = sig.uuid,
          name = sig.name,
          sourceUUID = ops.getEntityUUID(sig.source),
          targetUUID = ops.getEntityUUID(sig.target))
      }.to[Seq],
      (rr: oml.tables.ReifiedRelationshipRestriction) => rr.uuid)(taggedTypes.orderingReifiedRelationshipRestrictionUUID)

    allReifiedRelationships = parallelSort.parSortBy(
      s.reifiedRelationships.map { rr =>
        val sig = ops.fromReifiedRelationship(rr)
        oml.tables.ReifiedRelationship(
          tboxUUID = suuid,
          uuid = sig.uuid,
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
          sourceUUID = ops.getEntityUUID(sig.source),
          targetUUID = ops.getEntityUUID(sig.target))
      }.to[Seq],
      (rr: oml.tables.ReifiedRelationship) => rr.uuid)(taggedTypes.orderingReifiedRelationshipUUID)

    allForwardProperties = parallelSort.parSortBy(
      s.reifiedRelationships.map { rr =>
        val sig = ops.fromReifiedRelationship(rr)
        oml.tables.ForwardProperty(
          uuid = sig.forwardPropertyInfo.uuid,
          name = sig.forwardPropertyInfo.name,
          reifiedRelationshipUUID = sig.uuid)
      }.to[Seq],
      (f: oml.tables.ForwardProperty) => f.uuid)(taggedTypes.orderingForwardPropertyUUID)

    allInverseProperties = parallelSort.parSortBy(
      s.reifiedRelationships.flatMap { rr =>
        val sig = ops.fromReifiedRelationship(rr)
        sig.inversePropertyInfo.map { inv =>
          oml.tables.InverseProperty(
            uuid = inv.uuid,
            name = inv.name,
            reifiedRelationshipUUID = sig.uuid)
        }
      }.to[Seq],
      (i: oml.tables.InverseProperty) => i.uuid)(taggedTypes.orderingInversePropertyUUID)

    allUnreifiedRelationships = parallelSort.parSortBy(
      s.unreifiedRelationships.map { ur =>
        val sig = ops.fromUnreifiedRelationship(ur)
        oml.tables.UnreifiedRelationship(
          tboxUUID = suuid,
          uuid = sig.uuid,
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
          sourceUUID = ops.getEntityUUID(sig.source),
          targetUUID = ops.getEntityUUID(sig.target))
      }.to[Seq],
      (u: oml.tables.UnreifiedRelationship) => u.uuid)(taggedTypes.orderingUnreifiedRelationshipUUID)

    allScalars = parallelSort.parSortBy(
      s.scalarDataTypes.map { sc =>
        oml.tables.Scalar(
          tboxUUID = suuid,
          uuid = ops.getScalarUUID(sc),
          name = ops.getTermName(sc))
      }.to[Seq],
      (x: oml.tables.Scalar) => x.uuid)(taggedTypes.orderingScalarUUID)

    allStructures = parallelSort.parSortBy(
      s.structuredDataTypes.map { st =>
        oml.tables.Structure(
          tboxUUID = suuid,
          uuid = ops.getStructureUUID(st),
          name = ops.getTermName(st))
      }.to[Seq],
      (x: oml.tables.Structure) => x.uuid)(taggedTypes.orderingStructureUUID)

    allBinaryScalarRestrictions = parallelSort.parSortBy(
      s.binaryScalarRestrictions.map { dr =>
        val info = ops.fromBinaryScalarRestriction(dr)
        oml.tables.BinaryScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.BinaryScalarRestriction) => x.uuid)(taggedTypes.orderingBinaryScalarRestrictionUUID)

    allIRIScalarRestrictions = parallelSort.parSortBy(
      s.iriScalarRestrictions.map { dr =>
        val info = ops.fromIRIScalarRestriction(dr)
        oml.tables.IRIScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern.map(canonicalLiteralPattern),
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.IRIScalarRestriction) => x.uuid)(taggedTypes.orderingIRIScalarRestrictionUUID)

    allNumericScalarRestrictions = parallelSort.parSortBy(
      s.numericScalarRestrictions.map { dr =>
        val info = ops.fromNumericScalarRestriction(dr)
        oml.tables.NumericScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          maxExclusive = info.maxExclusive,
          maxInclusive = info.maxInclusive,
          minExclusive = info.minExclusive,
          minInclusive = info.minInclusive,
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.NumericScalarRestriction) => x.uuid)(taggedTypes.orderingNumericScalarRestrictionUUID)

    allPlainLiteralScalarRestrictions = parallelSort.parSortBy(
      s.plainLiteralScalarRestrictions.map { dr =>
        val info = ops.fromPlainLiteralScalarRestriction(dr)
        oml.tables.PlainLiteralScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          langRange = info.langRange,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern.map(canonicalLiteralPattern),
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.PlainLiteralScalarRestriction) => x.uuid)(taggedTypes.orderingPlainLiteralScalarRestrictionUUID)

    allScalarOneOfRestrictions = parallelSort.parSortBy(
      s.scalarOneOfRestrictions.map { dr =>
        val info = ops.fromScalarOneOfRestriction(dr)
        oml.tables.ScalarOneOfRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.ScalarOneOfRestriction) => x.uuid)(taggedTypes.orderingScalarOneOfRestrictionUUID)

    allScalarOneOfLiteralAxioms = parallelSort.parSortBy(
      s.scalarOneOfLiterals.map { ax =>
        val info = ops.fromScalarOneOfLiteralAxiom(ax)
        oml.tables.ScalarOneOfLiteralAxiom(
          tboxUUID = suuid,
          uuid = info.uuid,
          axiomUUID = ops.getScalarOneOfRestrictionUUID(info.restriction),
          value = info.value,
          valueTypeUUID = info.valueType.map(ops.getDataRangeUUID(_))
        )
      }.to[Seq],
      (x: oml.tables.ScalarOneOfLiteralAxiom) => x.uuid)(taggedTypes.orderingScalarOneOfLiteralAxiomUUID)

    allStringScalarRestrictions = parallelSort.parSortBy(
      s.stringScalarRestrictions.map { dr =>
        val info = ops.fromStringScalarRestriction(dr)
        oml.tables.StringScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          length = info.length,
          maxLength = info.maxLength,
          minLength = info.minLength,
          pattern = info.pattern.map(canonicalLiteralPattern),
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.StringScalarRestriction) => x.uuid)(taggedTypes.orderingStringScalarRestrictionUUID)

    allSynonymScalarRestrictions = parallelSort.parSortBy(
      s.synonymScalarRestrictions.map { dr =>
        val info = ops.fromSynonymScalarRestriction(dr)
        oml.tables.SynonymScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.SynonymScalarRestriction) => x.uuid)(taggedTypes.orderingSynonymScalarRestrictionUUID)

    allTimeScalarRestrictions = parallelSort.parSortBy(
      s.timeScalarRestrictions.map { dr =>
        val info = ops.fromTimeScalarRestriction(dr)
        oml.tables.TimeScalarRestriction(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          maxExclusive = info.maxExclusive,
          maxInclusive = info.maxInclusive,
          minExclusive = info.minExclusive,
          minInclusive = info.minInclusive,
          restrictedRangeUUID = ops.getDataRangeUUID(info.restrictedRange))
      }.to[Seq],
      (x: oml.tables.TimeScalarRestriction) => x.uuid)(taggedTypes.orderingTimeScalarRestrictionUUID)

    allEntity2ScalarProperties = parallelSort.parSortBy(
      s.entityScalarDataProperties.map { e2sc =>
        val info = ops.fromEntityScalarDataProperty(e2sc)
        oml.tables.EntityScalarDataProperty(
          tboxUUID = suuid,
          uuid = info.uuid,
          domainUUID = ops.getEntityUUID(info.domain),
          rangeUUID = ops.getDataRangeUUID(info.range),
          isIdentityCriteria = info.isIdentityCriteria,
          name = info.name)
      }.to[Seq],
      (x: oml.tables.EntityScalarDataProperty) => x.uuid)(taggedTypes.orderingEntityScalarDataPropertyUUID)

    allEntity2StructureProperties = parallelSort.parSortBy(
      s.entityStructuredDataProperties.map { e2sc =>
        val info = ops.fromEntityStructuredDataProperty(e2sc)
        oml.tables.EntityStructuredDataProperty(
          tboxUUID = suuid,
          uuid = info.uuid,
          domainUUID = ops.getEntityUUID(info.domain),
          rangeUUID = ops.getStructureUUID(info.range),
          isIdentityCriteria = info.isIdentityCriteria,
          name = info.name)
      }.to[Seq],
      (x: oml.tables.EntityStructuredDataProperty) => x.uuid)(taggedTypes.orderingEntityStructuredDataPropertyUUID)

    allScalarProperties = parallelSort.parSortBy(
      s.scalarDataProperties.map { s2sc =>
        val info = ops.fromScalarDataProperty(s2sc)
        oml.tables.ScalarDataProperty(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          domainUUID = ops.getStructureUUID(info.domain),
          rangeUUID = ops.getDataRangeUUID(info.range))
      }.to[Seq],
      (x: oml.tables.ScalarDataProperty) => x.uuid)(taggedTypes.orderingScalarDataPropertyUUID)

    allStructuredProperties = parallelSort.parSortBy(
      s.structuredDataProperties.map { s2sc =>
        val info = ops.fromStructuredDataProperty(s2sc)
        oml.tables.StructuredDataProperty(
          tboxUUID = suuid,
          uuid = info.uuid,
          name = info.name,
          domainUUID = ops.getStructureUUID(info.domain),
          rangeUUID = ops.getStructureUUID(info.range))
      }.to[Seq],
      (x: oml.tables.StructuredDataProperty) => x.uuid)(taggedTypes.orderingStructuredDataPropertyUUID)

    allChainRules = parallelSort.parSortBy(
      s.chainRules.map { cr =>
        val info = ops.fromChainRule(cr)
        oml.tables.ChainRule(
          uuid = info.uuid,
          tboxUUID = suuid,
          name = info.name,
          headUUID = ops.getRestrictableRelationshipUUID(info.head))
      }.to[Seq],
      (x: oml.tables.ChainRule) => x.uuid)(taggedTypes.orderingChainRuleUUID)

    allRuleBodySegments = parallelSort.parSortBy(
      s.ruleBodySegments.map { rbs =>
        val info = ops.fromRuleBodySegment(rbs)
        oml.tables.RuleBodySegment(
          uuid = info.uuid,
          previousSegmentUUID = info.previousSegment.map { prev =>
            ops.fromRuleBodySegment(prev).uuid
          },
          ruleUUID = info.chainRule.map { rule =>
            ops.fromChainRule(rule).uuid
          }
        )
      }.to[Seq],
      (x: oml.tables.RuleBodySegment) => x.uuid)(taggedTypes.orderingRuleBodySegmentUUID)

    allSegmentPredicates = parallelSort.parSortBy(
      s.segmentPredicates.map { p =>
        val info = ops.fromSegmentPredicate(p)
        oml.tables.SegmentPredicate(
          uuid = info.uuid,
          bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid,
          predicateUUID = info.predicate.map(p => ops.fromPredicate(p).uuid),
          reifiedRelationshipSourceUUID = info.reifiedRelationshipSource.map(rr => ops.fromReifiedRelationship(rr).uuid),
          reifiedRelationshipInverseSourceUUID = info.reifiedRelationshipInverseSource.map(rr => ops.fromReifiedRelationship(rr).uuid),
          reifiedRelationshipTargetUUID = info.reifiedRelationshipTarget.map(rr => ops.fromReifiedRelationship(rr).uuid),
          reifiedRelationshipInverseTargetUUID = info.reifiedRelationshipInverseTarget.map(rr => ops.fromReifiedRelationship(rr).uuid),
          unreifiedRelationshipInverseUUID = info.unreifiedRelationshipInverse.map(rr => ops.fromUnreifiedRelationship(rr).uuid))
      }.to[Seq],
      (x: oml.tables.SegmentPredicate) => x.uuid)(taggedTypes.orderingSegmentPredicateUUID)

    allAxioms = s.axioms.foldLeft(Axioms())(Axioms.combine(suuid, ops))

    allRootConceptTaxonomyAxioms = parallelSort.parSortBy(
      s.rTAxioms.map { ax =>
        val info = ops.fromRootConceptTaxonomyAxiom(ax)
        oml.tables.RootConceptTaxonomyAxiom(
          uuid = info.uuid,
          bundleUUID = suuid,
          rootUUID = ops.getConceptKindUUID(info.root))
      }.to[Seq],
      (x: oml.tables.RootConceptTaxonomyAxiom) => x.uuid)(taggedTypes.orderingRootConceptTaxonomyAxiomUUID)

    allSpecificDisjointConceptAxioms = parallelSort.parSortBy(
      s.sTAxioms.map { ax =>
        val info = ops.fromSpecificDisjointConceptAxiom(ax)
        oml.tables.SpecificDisjointConceptAxiom(
          uuid = info.uuid,
          disjointLeafUUID = ops.getConceptKindUUID(info.disjointLeaf),
          disjointTaxonomyParentUUID = ops.getConceptTreeDisjunctionUUID(info.disjointTaxonomyParent))
      }.to[Seq],
      (x: oml.tables.SpecificDisjointConceptAxiom) => x.uuid)(taggedTypes.orderingSpecificDisjointConceptAxiomUUID)

    allAnonymousConceptUnionAxioms = parallelSort.parSortBy(
      s.aTAxioms.map { ax =>
        val info = ops.fromAnonymousConceptTaxonomyAxiom(ax)
        oml.tables.AnonymousConceptUnionAxiom(
          uuid = info.uuid,
          disjointTaxonomyParentUUID = ops.getConceptTreeDisjunctionUUID(info.disjointTaxonomyParent),
          name = info.name)
      }.to[Seq],
      (x: oml.tables.AnonymousConceptUnionAxiom) => x.uuid)(taggedTypes.orderingAnonymousConceptUnionAxiomUUID)

    b = new oml.tables.Bundle(
      oug = oug,
      kind = if (TerminologyKind.isOpenWorldKind(s.kind))
        oml.tables.OpenWorldDefinitions
      else
        oml.tables.ClosedWorldDesignations,
      iri = s.iri)

    buuid = b.uuid
    _ <- {
      if (toUUIDString(suuid) == buuid)
        ().right[Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"tables.Bundle(kind=${s.kind}, iri=${s.iri}) UUID mismatch:\n input=$suuid\n derived=$buuid"
        )).left[Unit]
    }

    table = oml.tables.OMLSpecificationTables(
      terminologyGraphs = Seq.empty,
      bundles = Seq(b),
      descriptionBoxes = Seq.empty,

      annotationProperties = allAnnotationProperties,

      aspects = allAspects,
      concepts = allConcepts,

      scalars = allScalars,
      structures = allStructures,

      conceptDesignationTerminologyAxioms = allConceptDesignationTerminologyAxioms,
      terminologyExtensionAxioms = allExtensionAxioms,
      terminologyNestingAxioms = allNestingAxioms,
      bundledTerminologyAxioms = allBundledTerminologyAxioms,
      descriptionBoxExtendsClosedWorldDefinitions = Seq.empty,
      descriptionBoxRefinements = Seq.empty,

      binaryScalarRestrictions = allBinaryScalarRestrictions,
      iriScalarRestrictions = allIRIScalarRestrictions,
      numericScalarRestrictions = allNumericScalarRestrictions,
      plainLiteralScalarRestrictions = allPlainLiteralScalarRestrictions,
      scalarOneOfRestrictions = allScalarOneOfRestrictions,
      scalarOneOfLiteralAxioms = allScalarOneOfLiteralAxioms,
      stringScalarRestrictions = allStringScalarRestrictions,
      synonymScalarRestrictions = allSynonymScalarRestrictions,
      timeScalarRestrictions = allTimeScalarRestrictions,

      entityScalarDataProperties = allEntity2ScalarProperties,
      entityStructuredDataProperties = allEntity2StructureProperties,
      scalarDataProperties = allScalarProperties,
      structuredDataProperties = allStructuredProperties,

      reifiedRelationshipRestrictions = allReifiedRelationshipRestrictions,
      reifiedRelationships = allReifiedRelationships,
      forwardProperties = allForwardProperties,
      inverseProperties = allInverseProperties,
      unreifiedRelationships = allUnreifiedRelationships,

      chainRules = allChainRules,
      ruleBodySegments = allRuleBodySegments,
      segmentPredicates = allSegmentPredicates,

      aspectSpecializationAxioms =
        parallelSort.parSortBy(
          allAxioms.aspectSpecializationAxioms,
          (x: oml.tables.AspectSpecializationAxiom) => x.uuid)(taggedTypes.orderingAspectSpecializationAxiomUUID),

      conceptSpecializationAxioms =
        parallelSort.parSortBy(
          allAxioms.conceptSpecializationAxioms,
          (x: oml.tables.ConceptSpecializationAxiom) => x.uuid)(taggedTypes.orderingConceptSpecializationAxiomUUID),

      reifiedRelationshipSpecializationAxioms =
        parallelSort.parSortBy(
          allAxioms.reifiedRelationshipSpecializationAxioms,
          (x: oml.tables.ReifiedRelationshipSpecializationAxiom) => x.uuid)(taggedTypes.orderingReifiedRelationshipSpecializationAxiomUUID),

      subDataPropertyOfAxioms =
        parallelSort.parSortBy(
          allAxioms.subDataPropertyOfAxioms,
          (x: oml.tables.SubDataPropertyOfAxiom) => x.uuid)(taggedTypes.orderingSubDataPropertyOfAxiomUUID),

      subObjectPropertyOfAxioms =
        parallelSort.parSortBy(
          allAxioms.subObjectPropertyOfAxioms,
          (x: oml.tables.SubObjectPropertyOfAxiom) => x.uuid)(taggedTypes.orderingSubObjectPropertyOfAxiomUUID),

      entityExistentialRestrictionAxioms =
        parallelSort.parSortBy(
          allAxioms.entityExistentialRestrictionAxioms,
          (x: oml.tables.EntityExistentialRestrictionAxiom) => x.uuid)(taggedTypes.orderingEntityExistentialRestrictionAxiomUUID),

      entityUniversalRestrictionAxioms =
        parallelSort.parSortBy(
          allAxioms.entityUniversalRestrictionAxioms,
          (x: oml.tables.EntityUniversalRestrictionAxiom) => x.uuid)(taggedTypes.orderingEntityUniversalRestrictionAxiomUUID),

      entityScalarDataPropertyExistentialRestrictionAxioms =
        parallelSort.parSortBy(
          allAxioms.entityScalarDataPropertyExistentialRestrictionAxioms,
          (x: oml.tables.EntityScalarDataPropertyExistentialRestrictionAxiom) => x.uuid)(taggedTypes.orderingEntityScalarDataPropertyExistentialRestrictionAxiomUUID),

      entityScalarDataPropertyParticularRestrictionAxioms =
        parallelSort.parSortBy(
          allAxioms.entityScalarDataPropertyParticularRestrictionAxioms,
          (x: oml.tables.EntityScalarDataPropertyParticularRestrictionAxiom) => x.uuid)(taggedTypes.orderingEntityScalarDataPropertyParticularRestrictionAxiomUUID),

      entityScalarDataPropertyUniversalRestrictionAxioms =
        parallelSort.parSortBy(
          allAxioms.entityScalarDataPropertyUniversalRestrictionAxioms,
          (x: oml.tables.EntityScalarDataPropertyUniversalRestrictionAxiom) => x.uuid)(taggedTypes.orderingEntityScalarDataPropertyUniversalRestrictionAxiomUUID),

      rootConceptTaxonomyAxioms = allRootConceptTaxonomyAxioms,
      specificDisjointConceptAxioms = allSpecificDisjointConceptAxioms,
      anonymousConceptUnionAxioms = allAnonymousConceptUnionAxioms,

      annotationPropertyValues =
        parallelSort.parSortBy(
          s.annotationPropertyValues.to[Seq],
          (x: oml.tables.AnnotationPropertyValue) => x.uuid)(taggedTypes.orderingAnnotationPropertyValueUUID)
    )

  } yield im2st :+ (bundle -> table)

}
