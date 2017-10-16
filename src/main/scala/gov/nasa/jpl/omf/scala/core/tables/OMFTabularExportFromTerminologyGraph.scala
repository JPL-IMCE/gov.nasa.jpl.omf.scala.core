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
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{OMF, OMFError, OMFOps, RelationshipCharacteristics, TerminologyKind}

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMFTabularExportFromTerminologyGraph {

  def toTables[omf <: OMF]
  (acc: Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)])
  (tbox: omf#ImmutableTerminologyGraph)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)]
  = for {
    im2st <- acc
    all_tboxes = im2st.map(_._1).toSet[omf#Module]
    all_tables = im2st.map(_._2).to[Set]

    all_aps = all_tables.flatMap(_.annotationProperties)

    s = ops.fromImmutableTerminology(tbox)
    suuid = s.uuid.toString

    oug = oml.uuid.JVMUUIDGenerator()

    s_common_aps = s.annotationProperties intersect all_aps
    s_ap = s.annotationProperties -- s_common_aps

    // Check that there are no overlaping annotation properties
    _ = {
      if (s_common_aps.nonEmpty) {
        val common = s_common_aps.to[Seq].sortBy(_.abbrevIRI)
        System.out.println(
          s"TerminologyGraph ${s.iri} duplicates ${common.size} Annotations defined in imported modules: " +
            common.map(_.abbrevIRI).mkString("\n\t",", ","\n"))
      }
    }

    allConceptDesignationTerminologyAxioms <-
    s.conceptDesignation.foldLeft(
      Seq.empty[oml.tables.ConceptDesignationTerminologyAxiom].right[Throwables]
    ) { case ( acc1, omf_ax) =>
      for {
        axs <- acc1
        omf_info = ops.fromConceptDesignationTerminologyAxiom(omf_ax)
        _ <- if (all_tboxes.exists(i => ops.getModuleUUID(i) == ops.getModuleUUID(omf_info.designatedTerminology)))
          ().right[Throwables]
        else
          Set[java.lang.Throwable](OMFError.omfError(
            s"TerminologyGraph ${s.iri} has a ConceptDesignationTerminologyAxiom (uuid=${omf_info.uuid}) "+
              s" whose designated terminology is not imported: ${ops.getModuleIRI(omf_info.designatedTerminology)}"))
            .left[Unit]
        ax = oml.tables.ConceptDesignationTerminologyAxiom(
          uuid = omf_info.uuid.toString,
          tboxUUID = omf_info.graphUUID.toString,
          designatedConceptUUID = ops.getTermUUID(omf_info.designatedConcept).toString,
          designatedTerminologyIRI = ops.getModuleIRI(omf_info.designatedTerminology).toString)

      } yield axs :+ ax
    }

    allExtensionAxioms <-
    s.extensions.foldLeft(
      Seq.empty[oml.tables.TerminologyExtensionAxiom].right[Throwables]
    ) { case (acc1, omf_ax) =>
      for {
        axs <- acc1
        omf_info = ops.fromTerminologyExtensionAxiom(omf_ax)
        ax = oml.tables.TerminologyExtensionAxiom(
          uuid = omf_info.uuid.toString,
          tboxUUID = suuid, extendedTerminologyIRI = ops.getModuleIRI(omf_info.extendedTerminology).toString)

      } yield axs :+ ax
    }

    allNestingAxioms <-
    s.nesting.foldLeft(
      Seq.empty[oml.tables.TerminologyNestingAxiom].right[Throwables]
    ) { case (acc1, omf_ax) =>
      for {
        axs <- acc1
        omf_info = ops.fromTerminologyNestingAxiom(omf_ax)
        ax = oml.tables.TerminologyNestingAxiom(
          uuid = omf_info.uuid.toString,
          tboxUUID = suuid,
          nestingTerminologyIRI = ops.getModuleIRI(omf_info.nestingTerminology).toString,
          nestingContextUUID = ops.getTermUUID(omf_info.nestingContext).toString)

      } yield axs :+ ax
    }

    allAspects = s.aspects.map { a =>
      oml.tables.Aspect(
        tboxUUID = suuid,
        uuid = ops.getTermUUID(a).toString,
        name = ops.getTermName(a))
    }.to[Seq].sorted

    allConcepts = s.concepts.map { c =>
      oml.tables.Concept(
        tboxUUID = suuid,
        uuid = ops.getTermUUID(c).toString,
        name = ops.getTermName(c))
    }.to[Seq].sorted

    allReifiedRelationships = s.reifiedRelationships.map { rr =>
      val sig = ops.fromReifiedRelationship(rr)
      oml.tables.ReifiedRelationship(
        tboxUUID = suuid,
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
    }.to[Seq].sorted

    allUnreifiedRelationships = s.unreifiedRelationships.map { ur =>
      val sig = ops.fromUnreifiedRelationship(ur)
      oml.tables.UnreifiedRelationship(
        tboxUUID = suuid,
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
    }.to[Seq].sorted

    allScalars = s.scalarDataTypes.map { sc =>
      oml.tables.Scalar(
        tboxUUID = suuid,
        uuid = ops.getTermUUID(sc).toString,
        name = ops.getTermName(sc))
    }.to[Seq].sorted

    allStructures = s.structuredDataTypes.map { st =>
      oml.tables.Structure(
        tboxUUID = suuid,
        uuid = ops.getTermUUID(st).toString,
        name = ops.getTermName(st))
    }.to[Seq].sorted

    allBinaryScalarRestrictions = s.binaryScalarRestrictions.map { dr =>
      val info = ops.fromBinaryScalarRestriction(dr)
      oml.tables.BinaryScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        length = info.length,
        maxLength = info.maxLength,
        minLength = info.minLength,
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allIRIScalarRestrictions = s.iriScalarRestrictions.map { dr =>
      val info = ops.fromIRIScalarRestriction(dr)
      oml.tables.IRIScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        length = info.length,
        maxLength = info.maxLength,
        minLength = info.minLength,
        pattern = info.pattern.map(canonicalLiteralPattern),
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allNumericScalarRestrictions = s.numericScalarRestrictions.map { dr =>
      val info = ops.fromNumericScalarRestriction(dr)
      oml.tables.NumericScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        maxExclusive = info.maxExclusive,
        maxInclusive = info.maxInclusive,
        minExclusive = info.minExclusive,
        minInclusive = info.minInclusive,
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allPlainLiteralScalarRestrictions = s.plainLiteralScalarRestrictions.map { dr =>
      val info = ops.fromPlainLiteralScalarRestriction(dr)
      oml.tables.PlainLiteralScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        langRange = info.langRange,
        length = info.length,
        maxLength = info.maxLength,
        minLength = info.minLength,
        pattern = info.pattern.map(canonicalLiteralPattern),
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allScalarOneOfRestrictions = s.scalarOneOfRestrictions.map { dr =>
      val info = ops.fromScalarOneOfRestriction(dr)
      oml.tables.ScalarOneOfRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allStringScalarRestrictions = s.stringScalarRestrictions.map { dr =>
      val info = ops.fromStringScalarRestriction(dr)
      oml.tables.StringScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        length = info.length,
        maxLength = info.maxLength,
        minLength = info.minLength,
        pattern = info.pattern.map(canonicalLiteralPattern),
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allSynonymScalarRestrictions = s.synonymScalarRestrictions.map { dr =>
      val info = ops.fromSynonymScalarRestriction(dr)
      oml.tables.SynonymScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allTimeScalarRestrictions = s.timeScalarRestrictions.map { dr =>
      val info = ops.fromTimeScalarRestriction(dr)
      oml.tables.TimeScalarRestriction(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        maxExclusive = info.maxExclusive,
        maxInclusive = info.maxInclusive,
        minExclusive = info.minExclusive,
        minInclusive = info.minInclusive,
        restrictedRangeUUID = ops.getTermUUID(info.restrictedRange).toString)
    }.to[Seq].sorted

    allEntity2ScalarProperties = s.entityScalarDataProperties.map { e2sc =>
      val info = ops.fromEntityScalarDataProperty(e2sc)
      oml.tables.EntityScalarDataProperty(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        domainUUID = ops.getTermUUID(info.domain).toString,
        rangeUUID = ops.getTermUUID(info.range).toString,
        isIdentityCriteria = info.isIdentityCriteria,
        name = info.name)
    }.to[Seq].sorted

    allEntity2StructureProperties = s.entityStructuredDataProperties.map { e2sc =>
      val info = ops.fromEntityStructuredDataProperty(e2sc)
      oml.tables.EntityStructuredDataProperty(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        domainUUID = ops.getTermUUID(info.domain).toString,
        rangeUUID = ops.getTermUUID(info.range).toString,
        isIdentityCriteria = info.isIdentityCriteria,
        name = info.name)
    }.to[Seq].sorted

    allScalarProperties = s.scalarDataProperties.map { s2sc =>
      val info = ops.fromScalarDataProperty(s2sc)
      oml.tables.ScalarDataProperty(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        domainUUID = ops.getTermUUID(info.domain).toString,
        rangeUUID = ops.getTermUUID(info.range).toString)
    }.to[Seq].sorted

    allStructuredProperties = s.structuredDataProperties.map { s2sc =>
      val info = ops.fromStructuredDataProperty(s2sc)
      oml.tables.StructuredDataProperty(
        tboxUUID = suuid,
        uuid = info.uuid.toString,
        name = info.name,
        domainUUID = ops.getTermUUID(info.domain).toString,
        rangeUUID = ops.getTermUUID(info.range).toString)
    }.to[Seq].sorted

    allChainRules = s.chainRules.map { cr =>
      val info = ops.fromChainRule(cr)
      oml.tables.ChainRule(
        uuid = info.uuid.toString,
        tboxUUID = suuid,
        name = info.name,
        headUUID = ops.getTermUUID(info.head).toString)
    }.to[Seq].sorted

    allRuleBodySegments = s.ruleBodySegments.map { rbs =>
      val info = ops.fromRuleBodySegment(rbs)
      oml.tables.RuleBodySegment(
        uuid = info.uuid.toString,
        previousSegmentUUID = info.previousSegment.map { prev =>
          ops.fromRuleBodySegment(prev).uuid.toString
        },
        ruleUUID = info.chainRule.map { rule =>
          ops.fromChainRule(rule).uuid.toString
        }
      )
    }.to[Seq].sorted

    allAspectPredicates = s.aspectPredicates.map { p =>
      val info = ops.fromAspectPredicate(p)
      oml.tables.AspectPredicate(
        uuid = info.uuid.toString,
        aspectUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allConceptPredicates = s.conceptPredicates.map { p =>
      val info = ops.fromConceptPredicate(p)
      oml.tables.ConceptPredicate(
        uuid = info.uuid.toString,
        conceptUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipPredicates = s.reifiedRelationshipPredicates.map { p =>
      val info = ops.fromReifiedRelationshipPredicate(p)
      oml.tables.ReifiedRelationshipPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipPropertyPredicates = s.reifiedRelationshipPropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipPropertyPredicate(p)
      oml.tables.ReifiedRelationshipPropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipInversePropertyPredicates = s.reifiedRelationshipInversePropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipInversePropertyPredicate(p)
      oml.tables.ReifiedRelationshipInversePropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipSourcePropertyPredicates = s.reifiedRelationshipSourcePropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipSourcePropertyPredicate(p)
      oml.tables.ReifiedRelationshipSourcePropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipSourceInversePropertyPredicates = s.reifiedRelationshipSourceInversePropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipSourceInversePropertyPredicate(p)
      oml.tables.ReifiedRelationshipSourceInversePropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipTargetPropertyPredicates = s.reifiedRelationshipTargetPropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipTargetPropertyPredicate(p)
      oml.tables.ReifiedRelationshipTargetPropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allReifiedRelationshipTargetInversePropertyPredicates = s.reifiedRelationshipTargetInversePropertyPredicates.map { p =>
      val info = ops.fromReifiedRelationshipTargetInversePropertyPredicate(p)
      oml.tables.ReifiedRelationshipTargetInversePropertyPredicate(
        uuid = info.uuid.toString,
        reifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allUnreifiedRelationshipPropertyPredicates = s.unreifiedRelationshipPropertyPredicates.map { p =>
      val info = ops.fromUnreifiedRelationshipPropertyPredicate(p)
      oml.tables.UnreifiedRelationshipPropertyPredicate(
        uuid = info.uuid.toString,
        unreifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allUnreifiedRelationshipInversePropertyPredicates = s.unreifiedRelationshipInversePropertyPredicates.map { p =>
      val info = ops.fromUnreifiedRelationshipInversePropertyPredicate(p)
      oml.tables.UnreifiedRelationshipInversePropertyPredicate(
        uuid = info.uuid.toString,
        unreifiedRelationshipUUID = ops.getTermUUID(info.predicate).toString,
        bodySegmentUUID = ops.fromRuleBodySegment(info.bodySegment).uuid.toString)
    }.to[Seq].sorted

    allAxioms = s.axioms.foldLeft(Axioms())(Axioms.combine(suuid, ops))

    tg = new oml.tables.TerminologyGraph(
      oug = oug,
      kind = if (TerminologyKind.isOpenWorldKind(s.kind))
        oml.tables.OpenWorldDefinitions
      else
        oml.tables.ClosedWorldDesignations,
      iri = s.iri.toString)

    tuuid = tg.uuid
    _ <- {
      if (tuuid == suuid)
        ().right[Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"tables.TerminologyGraph(kind=${tg.kind}, iri=${tg.iri}) UUID mismatch:\n input=$suuid\n derived=${tg.uuid}"
        )).left[Unit]
    }

    table = oml.tables.OMLSpecificationTables.createEmptyOMLSpecificationTables()
      .copy(
        terminologyGraphs = Seq(tg),

        conceptDesignationTerminologyAxioms = allConceptDesignationTerminologyAxioms.sorted,
        terminologyExtensionAxioms = allExtensionAxioms.sorted,
        terminologyNestingAxioms = allNestingAxioms.sorted,

        aspects = allAspects,
        concepts = allConcepts,
        reifiedRelationships = allReifiedRelationships,
        unreifiedRelationships = allUnreifiedRelationships,
        scalars = allScalars,
        structures = allStructures,
        binaryScalarRestrictions = allBinaryScalarRestrictions,
        iriScalarRestrictions = allIRIScalarRestrictions,
        numericScalarRestrictions = allNumericScalarRestrictions,
        plainLiteralScalarRestrictions = allPlainLiteralScalarRestrictions,
        scalarOneOfRestrictions = allScalarOneOfRestrictions,
        stringScalarRestrictions = allStringScalarRestrictions,
        synonymScalarRestrictions = allSynonymScalarRestrictions,
        timeScalarRestrictions = allTimeScalarRestrictions,
        entityScalarDataProperties = allEntity2ScalarProperties,
        entityStructuredDataProperties = allEntity2StructureProperties,
        scalarDataProperties = allScalarProperties,
        structuredDataProperties = allStructuredProperties,

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

        chainRules = allChainRules,
        ruleBodySegments = allRuleBodySegments,

        aspectPredicates = allAspectPredicates,
        conceptPredicates = allConceptPredicates,
        reifiedRelationshipPredicates = allReifiedRelationshipPredicates,

        reifiedRelationshipPropertyPredicates = allReifiedRelationshipPropertyPredicates,
        reifiedRelationshipInversePropertyPredicates = allReifiedRelationshipInversePropertyPredicates,

        reifiedRelationshipSourcePropertyPredicates = allReifiedRelationshipSourcePropertyPredicates,
        reifiedRelationshipSourceInversePropertyPredicates = allReifiedRelationshipSourceInversePropertyPredicates,

        reifiedRelationshipTargetPropertyPredicates = allReifiedRelationshipTargetPropertyPredicates,
        reifiedRelationshipTargetInversePropertyPredicates = allReifiedRelationshipTargetInversePropertyPredicates,

        unreifiedRelationshipPropertyPredicates = allUnreifiedRelationshipPropertyPredicates,
        unreifiedRelationshipInversePropertyPredicates = allUnreifiedRelationshipInversePropertyPredicates,

        annotationProperties = s_ap.to[Seq].sortBy(_.uuid),

        annotationPropertyValues = s.annotationPropertyValues.to[Seq].sortBy(_.subjectUUID)
      )

  } yield im2st :+ (tbox -> table)

}
