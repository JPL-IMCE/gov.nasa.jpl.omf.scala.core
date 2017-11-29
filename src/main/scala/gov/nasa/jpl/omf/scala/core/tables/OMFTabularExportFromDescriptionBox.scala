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
import gov.nasa.jpl.imce.oml.resolver.toUUIDString
import gov.nasa.jpl.imce.oml.tables.taggedTypes
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{DescriptionKind, OMF, OMFError, OMFOps}

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMFTabularExportFromDescriptionBox {

  implicit def toIRI[omf <: OMF](iri: omf#IRI): taggedTypes.IRI = taggedTypes.iri(iri.toString)

  def toTables[omf <: OMF]
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

    s_common_aps = s.annotationProperties intersect all_aps
    s_ap = s.annotationProperties -- s_common_aps

    // Check that there are no overlaping annotation properties
    _ = {
      if (s_common_aps.nonEmpty) {
        val common = s_common_aps.to[Seq].sortBy(_.abbrevIRI.toString)
        System.out.println(
          s"TerminologyGraph ${s.iri} duplicates ${common.size} Annotations defined in imported modules: " +
            common.map(_.abbrevIRI).mkString("\n\t",", ","\n"))
      }
    }

    allClosedWorldDefinitions <-
    s.closedWorldDefinitions.foldLeft(
      Seq.empty[oml.tables.DescriptionBoxExtendsClosedWorldDefinitions].right[Throwables]
    ) { case (acc1, omf_ax) =>
      for {
        axs <- acc1
        omf_info = ops.fromClosedWorldDefinitionsAxiom(omf_ax)
        _ <- if (all_tboxes.contains(omf_info.extendedClosedWorldDefinitions))
          ().right[Throwables]
        else
          Set[java.lang.Throwable](OMFError.omfError(
            s"DescriptionBox ${s.iri} has a DescriptionBoxExtendsClosedWorldDefinitions (uuid=${omf_info.uuid}) "+
              s" whose extended closed-world definitions terminology is not imported: "+
              ops.getModuleIRI(omf_info.extendedClosedWorldDefinitions)))
            .left[Unit]
        ax = oml.tables.DescriptionBoxExtendsClosedWorldDefinitions(
          uuid = omf_info.uuid,
          descriptionBoxUUID = suuid,
          closedWorldDefinitionsIRI = ops.getModuleIRI(omf_info.extendedClosedWorldDefinitions))

      } yield axs :+ ax
    }

    allDescriptionBoxRefinements <-
    s.descriptionBoxRefinements.foldLeft(
      Seq.empty[oml.tables.DescriptionBoxRefinement].right[Throwables]
    ) { case (acc1, omf_ax) =>
      for {
        axs <- acc1
        omf_info = ops.fromDescriptionBoxRefinementAxiom(omf_ax)
        _ <- if (all_dboxes.contains(omf_info.refinedDescriptionBox))
          ().right[Throwables]
        else
          Set[java.lang.Throwable](OMFError.omfError(
            s"DescriptionBox ${s.iri} has a DescriptionBoxRefinement (uuid=${omf_info.uuid}) "+
              s" whose refined description is not imported: ${ops.getModuleIRI(omf_info.refinedDescriptionBox)}"))
            .left[Unit]
        ax = oml.tables.DescriptionBoxRefinement(
          uuid = omf_info.uuid,
          refiningDescriptionBoxUUID = omf_info.descriptionBox,
          refinedDescriptionBoxIRI = ops.getModuleIRI(omf_info.refinedDescriptionBox))

      } yield axs :+ ax
    }

    allConceptInstances =
    s.conceptInstances.map { i =>
      val s = ops.fromConceptInstance(i)
      oml.tables.ConceptInstance(
        uuid = s.uuid,
        descriptionBoxUUID = suuid,
        singletonConceptClassifierUUID = ops.getConceptUUID(s.concept),
        name = ops.getTermName(s.concept))
    }.to[Seq].sorted

    allReifiedRelationshipInstances =
    s.reifiedRelationshipInstances.map { i =>
      val info = ops.fromReifiedRelationshipInstance(i)
      oml.tables.ReifiedRelationshipInstance(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        singletonReifiedRelationshipClassifierUUID = ops.getReifiedRelationshipUUID(info.reifiedRelationshipClassifier),
        name = ops.getTermName(info.reifiedRelationshipClassifier))
    }.to[Seq].sorted

    allReifiedRelationshipInstanceDomains =
    s.reifiedRelationshipInstanceDomains.map { i =>
      val info = ops.fromReifiedRelationshipInstanceDomain(i)
      oml.tables.ReifiedRelationshipInstanceDomain(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        reifiedRelationshipInstanceUUID = ops.getReifiedRelationshipInstanceUUID(info.reifiedRelationshipInstance),
        domainUUID = ops.getConceptualEntitySingletonInstanceUUID(info.domain))
    }.to[Seq].sorted

    allReifiedRelationshipInstanceRanges =
    s.reifiedRelationshipInstanceRanges.map { i =>
      val info = ops.fromReifiedRelationshipInstanceRange(i)
      oml.tables.ReifiedRelationshipInstanceRange(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        reifiedRelationshipInstanceUUID = ops.getReifiedRelationshipInstanceUUID(info.reifiedRelationshipInstance),
        rangeUUID = ops.getConceptualEntitySingletonInstanceUUID(info.range))
    }.to[Seq].sorted

    allScalarDataPropertyValues =
    s.scalarDataPropertyValues.map { i =>
      val info = ops.fromScalarDataPropertyValue(i)
      oml.tables.ScalarDataPropertyValue(
        uuid = info.uuid,
        scalarDataPropertyUUID = ops.getDataRelationshipToScalarUUID(info.scalarDataProperty),
        scalarPropertyValue = info.scalarPropertyValue,
        structuredDataPropertyContextUUID = ops.getSingletonInstanceStructuredDataPropertyContextUUID(
          info.singletonInstanceStructuredDataPropertyContext),
        valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) })
    }.to[Seq].sorted

    allStructuredDataPropertyTuples =
    s.structuredDataPropertyTuples.map { i =>
      val info = ops.fromStructuredDataPropertyTuple(dbox, i)
      oml.tables.StructuredDataPropertyTuple(
        uuid = info.uuid,
        structuredDataPropertyUUID = ops.getDataRelationshipToStructureUUID(info.structuredataProperty),
        structuredDataPropertyContextUUID = ops.getSingletonInstanceStructuredDataPropertyContextUUID(
          info.singletonInstanceStructuredDataPropertyContext))
    }.to[Seq].sorted

    allSingletonInstanceScalarDataPropertyValues =
    s.singletonScalarDataPropertyValues.map { i =>
      val info = ops.fromSingletonInstanceScalarDataPropertyValue(i)
      oml.tables.SingletonInstanceScalarDataPropertyValue(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        singletonInstanceUUID = ops.getConceptualEntitySingletonInstanceUUID(info.singletonInstance),
        scalarDataPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.scalarDataProperty),
        scalarPropertyValue = info.scalarDataPropertyValue,
        valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) })
    }.to[Seq].sorted

    allSingletonInstanceStructuredDataPropertyValues =
    s.singletonStructuredDataPropertyValues.map { i =>
      val info = ops.fromSingletonInstanceStructuredDataPropertyValue(dbox, i)
      oml.tables.SingletonInstanceStructuredDataPropertyValue(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        singletonInstanceUUID = ops.getConceptualEntitySingletonInstanceUUID(info.singletonInstance),
        structuredDataPropertyUUID = ops.getDataRelationshipToStructureUUID(info.structuredDataProperty))
    }.to[Seq].sorted

    allUnreifiedRelationshipInstanceTuples =
    s.unreifiedRelationshipInstanceTuples.map { i =>
      val info = ops.fromUnreifiedRelationshipInstanceTuple(i)
      oml.tables.UnreifiedRelationshipInstanceTuple(
        uuid = info.uuid,
        descriptionBoxUUID = suuid,
        unreifiedRelationshipUUID = ops.getUnreifiedRelationshipUUID(info.unreifiedRelationship),
        domainUUID = ops.getConceptualEntitySingletonInstanceUUID(info.domain),
        rangeUUID = ops.getConceptualEntitySingletonInstanceUUID(info.range))
    }.to[Seq].sorted

    table = oml.tables.OMLSpecificationTables.createEmptyOMLSpecificationTables()
      .copy(
        descriptionBoxes = Seq(oml.tables.DescriptionBox(
          uuid = suuid,
          kind = if (DescriptionKind.isFinalKind(s.kind))
            oml.tables.Final
          else
            oml.tables.Partial,
          iri = s.iri)),

        descriptionBoxExtendsClosedWorldDefinitions = allClosedWorldDefinitions.sorted,
        descriptionBoxRefinements = allDescriptionBoxRefinements.sorted,

        conceptInstances = allConceptInstances,
        reifiedRelationshipInstances = allReifiedRelationshipInstances,
        reifiedRelationshipInstanceDomains = allReifiedRelationshipInstanceDomains,
        reifiedRelationshipInstanceRanges = allReifiedRelationshipInstanceRanges,
        scalarDataPropertyValues = allScalarDataPropertyValues,
        singletonInstanceScalarDataPropertyValues = allSingletonInstanceScalarDataPropertyValues,
        singletonInstanceStructuredDataPropertyValues = allSingletonInstanceStructuredDataPropertyValues,
        structuredDataPropertyTuples = allStructuredDataPropertyTuples,
        unreifiedRelationshipInstanceTuples = allUnreifiedRelationshipInstanceTuples,

        annotationProperties = s_ap.to[Seq].sorted,

        annotationPropertyValues = s.annotationPropertyValues.to[Seq].sorted
      )

  } yield im2st :+ (dbox -> table)

}
