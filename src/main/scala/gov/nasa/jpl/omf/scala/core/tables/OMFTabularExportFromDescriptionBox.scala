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
import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString
import gov.nasa.jpl.imce.oml.tables.taggedTypes
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{DescriptionKind, OMF, OMFError, OMFOps}

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMFTabularExportFromDescriptionBox {

  implicit def toIRI[omf <: OMF[omf]](iri: omf#IRI): taggedTypes.IRI = taggedTypes.iri(iri.toString)

  def toTables[omf <: OMF[omf]]
  (acc: Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)])
  (dbox: omf#ImmutableDescriptionBox)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)]
  = for {
    im2st <- acc
    all_tboxes = im2st.map(_._1).filter(ops.foldImmutableTerminologyBox(_).isDefined).toSet[omf#Module]
    all_dboxes = im2st.map(_._1).filter(ops.foldImmutableDescriptionBox(_).isDefined).toSet[omf#Module]

    all_tables = im2st.map(_._2).to[Set]

    all_aps = all_tables.flatMap(_.annotationProperties)

    s = ops.fromImmutableDescriptionBox(dbox)
    suuid = s.uuid

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
          s"DescriptionBox ${s.iri} duplicates ${common.size} Annotations defined in imported modules: " +
            common.map(_.abbrevIRI).mkString("\n\t", ", ", "\n"))
      }
    }

    closedWorldDefinitions <-
      s.closedWorldDefinitions.foldLeft(
        Seq.empty[oml.tables.DescriptionBoxExtendsClosedWorldDefinitions].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromClosedWorldDefinitionsAxiom(omf_ax)
          _ <- if (all_tboxes.exists { m => ops.getModuleIRI(m) == omf_info.extendedClosedWorldDefinitions })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"DescriptionBox ${s.iri} has a DescriptionBoxExtendsClosedWorldDefinitions (uuid=${omf_info.uuid}) " +
                s" whose extended closed-world definitions terminology is not imported: " +
                omf_info.extendedClosedWorldDefinitions))
              .left[Unit]
          ax = oml.tables.DescriptionBoxExtendsClosedWorldDefinitions(
            uuid = omf_info.uuid,
            descriptionBoxUUID = suuid,
            closedWorldDefinitionsIRI = omf_info.extendedClosedWorldDefinitions)

        } yield axs :+ ax
      }
    allClosedWorldDefinitions = parallelSort.parSortBy(
      closedWorldDefinitions,
      (x: oml.tables.DescriptionBoxExtendsClosedWorldDefinitions) => x.uuid)(taggedTypes.orderingDescriptionBoxExtendsClosedWorldDefinitionsUUID)

    descriptionBoxRefinements <-
      s.descriptionBoxRefinements.foldLeft(
        Seq.empty[oml.tables.DescriptionBoxRefinement].right[Throwables]
      ) { case (acc1, omf_ax) =>
        for {
          axs <- acc1
          omf_info = ops.fromDescriptionBoxRefinementAxiom(omf_ax)
          _ <- if (all_dboxes.exists { m => ops.getModuleIRI(m) == omf_info.refinedDescriptionBox })
            ().right[Throwables]
          else
            Set[java.lang.Throwable](OMFError.omfError(
              s"DescriptionBox ${s.iri} has a DescriptionBoxRefinement (uuid=${omf_info.uuid}) " +
                s" whose refined description is not imported: ${omf_info.refinedDescriptionBox}"))
              .left[Unit]
          ax = oml.tables.DescriptionBoxRefinement(
            uuid = omf_info.uuid,
            refiningDescriptionBoxUUID = omf_info.descriptionBox,
            refinedDescriptionBoxIRI = omf_info.refinedDescriptionBox)

        } yield axs :+ ax
      }
    allDescriptionBoxRefinements = parallelSort.parSortBy(
      descriptionBoxRefinements,
      (x: oml.tables.DescriptionBoxRefinement) => x.uuid)(taggedTypes.orderingDescriptionBoxRefinementUUID)

    allConceptInstances = parallelSort.parSortBy(
      s.conceptInstances.map { i =>
        val s = ops.fromConceptInstance(i)
        oml.tables.ConceptInstance(
          uuid = s.uuid,
          descriptionBoxUUID = suuid,
          singletonConceptClassifierUUID = ops.getConceptUUID(s.concept),
          name = ops.getTermName(s.concept))
      }.to[Seq],
      (x: oml.tables.ConceptInstance) => x.uuid)(taggedTypes.orderingConceptInstanceUUID)

    allReifiedRelationshipInstances = parallelSort.parSortBy(
      s.reifiedRelationshipInstances.map { i =>
        val info = ops.fromReifiedRelationshipInstance(i)
        oml.tables.ReifiedRelationshipInstance(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          singletonConceptualRelationshipClassifierUUID = ops.getConceptualRelationshipUUID(info.singletonConceptualRelationshipClassifier),
          name = ops.getTermName(info.singletonConceptualRelationshipClassifier))
      }.to[Seq],
      (x: oml.tables.ReifiedRelationshipInstance) => x.uuid)(taggedTypes.orderingReifiedRelationshipInstanceUUID)

    allReifiedRelationshipInstanceDomains = parallelSort.parSortBy(
      s.reifiedRelationshipInstanceDomains.map { i =>
        val info = ops.fromReifiedRelationshipInstanceDomain(i)
        oml.tables.ReifiedRelationshipInstanceDomain(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          reifiedRelationshipInstanceUUID = ops.getReifiedRelationshipInstanceUUID(info.reifiedRelationshipInstance),
          domainUUID = ops.getConceptualEntitySingletonInstanceUUID(info.domain))
      }.to[Seq],
      (x: oml.tables.ReifiedRelationshipInstanceDomain) => x.uuid)(taggedTypes.orderingReifiedRelationshipInstanceDomainUUID)

    allReifiedRelationshipInstanceRanges = parallelSort.parSortBy(
      s.reifiedRelationshipInstanceRanges.map { i =>
        val info = ops.fromReifiedRelationshipInstanceRange(i)
        oml.tables.ReifiedRelationshipInstanceRange(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          reifiedRelationshipInstanceUUID = ops.getReifiedRelationshipInstanceUUID(info.reifiedRelationshipInstance),
          rangeUUID = ops.getConceptualEntitySingletonInstanceUUID(info.range))
      }.to[Seq],
      (x: oml.tables.ReifiedRelationshipInstanceRange) => x.uuid)(taggedTypes.orderingReifiedRelationshipInstanceRangeUUID)

    allScalarDataPropertyValues = parallelSort.parSortBy(
      s.scalarDataPropertyValues.map { i =>
        val info = ops.fromScalarDataPropertyValue(i)
        oml.tables.ScalarDataPropertyValue(
          uuid = info.uuid,
          scalarDataPropertyUUID = ops.getDataRelationshipToScalarUUID(info.scalarDataProperty),
          scalarPropertyValue = info.scalarPropertyValue,
          structuredDataPropertyContextUUID = ops.getSingletonInstanceStructuredDataPropertyContextUUID(
            info.singletonInstanceStructuredDataPropertyContext),
          valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) })
      }.to[Seq],
      (x: oml.tables.ScalarDataPropertyValue) => x.uuid)(taggedTypes.orderingScalarDataPropertyValueUUID)

    allStructuredDataPropertyTuples = parallelSort.parSortBy(
      s.structuredDataPropertyTuples.map { i =>
        val info = ops.fromStructuredDataPropertyTuple(dbox, i)
        oml.tables.StructuredDataPropertyTuple(
          uuid = info.uuid,
          structuredDataPropertyUUID = ops.getDataRelationshipToStructureUUID(info.structuredataProperty),
          structuredDataPropertyContextUUID = ops.getSingletonInstanceStructuredDataPropertyContextUUID(
            info.singletonInstanceStructuredDataPropertyContext))
      }.to[Seq],
      (x: oml.tables.StructuredDataPropertyTuple) => x.uuid)(taggedTypes.orderingStructuredDataPropertyTupleUUID)

    allSingletonInstanceScalarDataPropertyValues = parallelSort.parSortBy(
      s.singletonScalarDataPropertyValues.map { i =>
        val info = ops.fromSingletonInstanceScalarDataPropertyValue(i)
        oml.tables.SingletonInstanceScalarDataPropertyValue(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          singletonInstanceUUID = ops.getConceptualEntitySingletonInstanceUUID(info.singletonInstance),
          scalarDataPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.scalarDataProperty),
          scalarPropertyValue = info.scalarDataPropertyValue,
          valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) })
      }.to[Seq],
      (x: oml.tables.SingletonInstanceScalarDataPropertyValue) => x.uuid)(taggedTypes.orderingSingletonInstanceScalarDataPropertyValueUUID)

    allSingletonInstanceStructuredDataPropertyValues = parallelSort.parSortBy(
      s.singletonStructuredDataPropertyValues.map { i =>
        val info = ops.fromSingletonInstanceStructuredDataPropertyValue(dbox, i)
        oml.tables.SingletonInstanceStructuredDataPropertyValue(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          singletonInstanceUUID = ops.getConceptualEntitySingletonInstanceUUID(info.singletonInstance),
          structuredDataPropertyUUID = ops.getDataRelationshipToStructureUUID(info.structuredDataProperty))
      }.to[Seq],
      (x: oml.tables.SingletonInstanceStructuredDataPropertyValue) => x.uuid)(taggedTypes.orderingSingletonInstanceStructuredDataPropertyValueUUID)

    allUnreifiedRelationshipInstanceTuples = parallelSort.parSortBy(
      s.unreifiedRelationshipInstanceTuples.map { i =>
        val info = ops.fromUnreifiedRelationshipInstanceTuple(i)
        oml.tables.UnreifiedRelationshipInstanceTuple(
          uuid = info.uuid,
          descriptionBoxUUID = suuid,
          unreifiedRelationshipUUID = ops.getUnreifiedRelationshipUUID(info.unreifiedRelationship),
          domainUUID = ops.getConceptualEntitySingletonInstanceUUID(info.domain),
          rangeUUID = ops.getConceptualEntitySingletonInstanceUUID(info.range))
      }.to[Seq],
      (x: oml.tables.UnreifiedRelationshipInstanceTuple) => x.uuid)(taggedTypes.orderingUnreifiedRelationshipInstanceTupleUUID)

    db = new oml.tables.DescriptionBox(
      oug = oug,
      kind = if (DescriptionKind.isFinalKind(s.kind))
        oml.tables.Final
      else
        oml.tables.Partial,
      iri = s.iri)

    duuid = db.uuid
    _ <- {
      if (toUUIDString(suuid) == duuid)
        ().right[Throwables]
      else
        Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
          s"tables.DescriptionBox(kind=${s.kind}, iri=${s.iri}) UUID mismatch:\n input=$suuid\n derived=$duuid"
        )).left[Unit]
    }

    table = oml.tables.OMLSpecificationTables(
      terminologyGraphs = Seq.empty,
      bundles = Seq.empty,
      descriptionBoxes = Seq(db),

      annotationProperties = allAnnotationProperties,

      descriptionBoxExtendsClosedWorldDefinitions = allClosedWorldDefinitions,
      descriptionBoxRefinements = allDescriptionBoxRefinements,

      conceptInstances = allConceptInstances,
      reifiedRelationshipInstances = allReifiedRelationshipInstances,
      reifiedRelationshipInstanceDomains = allReifiedRelationshipInstanceDomains,
      reifiedRelationshipInstanceRanges = allReifiedRelationshipInstanceRanges,
      scalarDataPropertyValues = allScalarDataPropertyValues,
      singletonInstanceScalarDataPropertyValues = allSingletonInstanceScalarDataPropertyValues,
      singletonInstanceStructuredDataPropertyValues = allSingletonInstanceStructuredDataPropertyValues,
      structuredDataPropertyTuples = allStructuredDataPropertyTuples,
      unreifiedRelationshipInstanceTuples = allUnreifiedRelationshipInstanceTuples,

      annotationPropertyValues =
        parallelSort.parSortBy(
          s.annotationPropertyValues.to[Seq],
          (x: oml.tables.AnnotationPropertyValue) => x.uuid)(taggedTypes.orderingAnnotationPropertyValueUUID)
    )

  } yield im2st :+ (dbox -> table)

}
