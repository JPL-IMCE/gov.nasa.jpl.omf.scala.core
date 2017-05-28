package gov.nasa.jpl.omf.scala.core.tables

import gov.nasa.jpl.imce.oml
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{DescriptionKind, OMF, OMFError, OMFOps}

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object OMFTabularExportFromDescriptionBox {

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

    s = ops.fromImmutableDescription(dbox)
    suuid = s.uuid.toString

    // Check that there are no overlaping annotation properties
    _ <- {
      val ap_overlap = all_aps intersect s.annotationProperties
      if (ap_overlap.isEmpty)
        ().right[Throwables]
      else
        Set[java.lang.Throwable](OMFError.omfError(
          s"DescriptionBox ${s.iri} duplicates ${ap_overlap.size} AnnotationProperties defined in imported modules: "+
            ap_overlap.map(_.abbrevIRI).mkString(","))).left[Unit]
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
          uuid = omf_info.uuid.toString,
          descriptionBoxUUID = suuid,
          closedWorldDefinitionsUUID = ops.getModuleUUID(omf_info.extendedClosedWorldDefinitions).toString)

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
          uuid = omf_info.uuid.toString,
          refiningDescriptionBoxUUID = omf_info.descriptionBox.toString,
          refinedDescriptionBoxUUID = ops.getModuleUUID(omf_info.refinedDescriptionBox).toString)

      } yield axs :+ ax
    }

    allConceptInstances =
    s.conceptInstances.map { i =>
      val s = ops.fromConceptInstance(i)
      oml.tables.ConceptInstance(
        uuid = s.uuid.toString,
        descriptionBoxUUID = suuid,
        singletonConceptClassifierUUID = ops.getTermUUID(s.concept).toString,
        name = ops.getTermName(s.concept))
    }.to[Seq].sorted

    allReifiedRelationshipInstances =
    s.reifiedRelationshipInstances.map { i =>
      val info = ops.fromReifiedRelationshipInstance(i)
      oml.tables.ReifiedRelationshipInstance(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        singletonReifiedRelationshipClassifierUUID = ops.getTermUUID(info.reifiedRelationshipClassifier).toString,
        name = ops.getTermName(info.reifiedRelationshipClassifier))
    }.to[Seq].sorted

    allReifiedRelationshipInstanceDomains =
    s.reifiedRelationshipInstanceDomains.map { i =>
      val info = ops.fromReifiedRelationshipInstanceDomain(i)
      oml.tables.ReifiedRelationshipInstanceDomain(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        reifiedRelationshipInstanceUUID = ops.getElementUUID(info.reifiedRelationshipInstance).toString,
        domainUUID = ops.getElementUUID(info.domain).toString)
    }.to[Seq].sorted

    allReifiedRelationshipInstanceRanges =
    s.reifiedRelationshipInstanceRanges.map { i =>
      val info = ops.fromReifiedRelationshipInstanceRange(i)
      oml.tables.ReifiedRelationshipInstanceRange(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        reifiedRelationshipInstanceUUID = ops.getElementUUID(info.reifiedRelationshipInstance).toString,
        rangeUUID = ops.getElementUUID(info.range).toString)
    }.to[Seq].sorted

    allScalarDataPropertyValues =
    s.scalarDataPropertyValues.map { i =>
      val info = ops.fromScalarDataPropertyValue(i)
      oml.tables.ScalarDataPropertyValue(
        uuid = info.uuid.toString,
        scalarDataPropertyUUID = ops.getTermUUID(info.scalarDataProperty).toString,
        scalarPropertyValue = info.scalarPropertyValue,
        structuredDataPropertyContextUUID = info.singletonInstanceStructuredDataPropertyContextUUID.toString)
    }.to[Seq].sorted

    allStructuredDataPropertyTuples =
    s.structuredDataPropertyTuples.map { i =>
      val info = ops.fromStructuredDataPropertyTuple(dbox, i)
      oml.tables.StructuredDataPropertyTuple(
        uuid = info.uuid.toString,
        structuredDataPropertyUUID = ops.getTermUUID(info.structuredataProperty).toString,
        structuredDataPropertyContextUUID = info.singletonInstanceStructuredDataPropertyContextUUID.toString)
    }.to[Seq].sorted

    allSingletonInstanceScalarDataPropertyValues =
    s.singletonScalarDataPropertyValues.map { i =>
      val info = ops.fromSingletonInstanceScalarDataPropertyValue(i)
      oml.tables.SingletonInstanceScalarDataPropertyValue(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        singletonInstanceUUID = ops.getElementUUID(info.singletonInstance).toString,
        scalarDataPropertyUUID = ops.getTermUUID(info.scalarDataProperty).toString,
        scalarPropertyValue = info.scalarDataPropertyValue)
    }.to[Seq].sorted

    allSingletonInstanceStructuredDataPropertyValues =
    s.singletonStructuredDataPropertyValues.map { i =>
      val info = ops.fromSingletonInstanceStructuredDataPropertyValue(dbox, i)
      oml.tables.SingletonInstanceStructuredDataPropertyValue(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        singletonInstanceUUID = ops.getElementUUID(info.singletonInstance).toString,
        structuredDataPropertyUUID = ops.getTermUUID(info.structuredDataProperty).toString)
    }.to[Seq].sorted

    allUnreifiedRelationshipInstanceTuples =
    s.unreifiedRelationshipInstanceTuples.map { i =>
      val info = ops.fromUnreifiedRelationshipInstanceTuple(i)
      oml.tables.UnreifiedRelationshipInstanceTuple(
        uuid = info.uuid.toString,
        descriptionBoxUUID = suuid,
        unreifiedRelationshipUUID = ops.getTermUUID(info.unreifiedRelationship).toString,
        domainUUID = ops.getElementUUID(info.domain).toString,
        rangeUUID = ops.getElementUUID(info.range).toString)
    }.to[Seq].sorted

    table = oml.tables.OMLSpecificationTables.createEmptyOMLSpecificationTables()
      .copy(
        annotationProperties = s.annotationProperties.to[Seq].sortBy(_.uuid),

        descriptionBoxes = Seq(oml.tables.DescriptionBox(
          uuid = suuid,
          kind = if (DescriptionKind.isFinalKind(s.kind))
            oml.tables.Final
          else
            oml.tables.Partial,
          iri = s.iri.toString)),

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
        annotations = s.annotations.map { case (ap, aes) => ap -> aes.to[Seq].sortBy(_.subjectUUID) }.toMap
      )

  } yield im2st :+ (dbox -> table)

}
