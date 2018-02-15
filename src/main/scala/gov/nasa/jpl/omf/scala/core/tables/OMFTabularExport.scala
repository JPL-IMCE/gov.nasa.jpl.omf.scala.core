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

import java.util.UUID

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.imce._
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.covariantTag
import gov.nasa.jpl.imce.oml.covariantTag.@@
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables

import scala.collection.immutable.Seq
import scala.Predef.String
import scalaz._
import Scalaz._

case class Axioms
( aspectSpecializationAxioms
  : Seq[oml.tables.AspectSpecializationAxiom] = Seq.empty,
  conceptSpecializationAxioms
  : Seq[oml.tables.ConceptSpecializationAxiom] = Seq.empty,
  reifiedRelationshipSpecializationAxioms
  : Seq[oml.tables.ReifiedRelationshipSpecializationAxiom] = Seq.empty,

  subDataPropertyOfAxioms
  : Seq[oml.tables.SubDataPropertyOfAxiom] = Seq.empty,
  subObjectPropertyOfAxioms
  : Seq[oml.tables.SubObjectPropertyOfAxiom] = Seq.empty,

  entityExistentialRestrictionAxioms
  : Seq[oml.tables.EntityExistentialRestrictionAxiom] = Seq.empty,
  entityUniversalRestrictionAxioms
  : Seq[oml.tables.EntityUniversalRestrictionAxiom] = Seq.empty,

  entityScalarDataPropertyExistentialRestrictionAxioms
  : Seq[oml.tables.EntityScalarDataPropertyExistentialRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyParticularRestrictionAxioms
  : Seq[oml.tables.EntityScalarDataPropertyParticularRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyUniversalRestrictionAxioms
  : Seq[oml.tables.EntityScalarDataPropertyUniversalRestrictionAxiom] = Seq.empty,

  entityStructuredDataPropertyParticularRestrictionAxioms
  : Seq[oml.tables.EntityStructuredDataPropertyParticularRestrictionAxiom] = Seq.empty,

  scalarOneOfLiteralAxioms
  : Seq[oml.tables.ScalarOneOfLiteralAxiom] = Seq.empty )

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

    subDataPropertyOfAxioms =
      a1.subDataPropertyOfAxioms ++ a2.subDataPropertyOfAxioms,
    subObjectPropertyOfAxioms =
      a1.subObjectPropertyOfAxioms ++ a2.subObjectPropertyOfAxioms,

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

    entityStructuredDataPropertyParticularRestrictionAxioms =
      a1.entityStructuredDataPropertyParticularRestrictionAxioms ++ a2.entityStructuredDataPropertyParticularRestrictionAxioms,

    scalarOneOfLiteralAxioms =
      a1.scalarOneOfLiteralAxioms ++ a2.scalarOneOfLiteralAxioms
  )

  implicit def toUUIDString[Tag](uuid: UUID @@ Tag)
  : String @@ Tag
  = covariantTag[Tag][String](uuid.toString)

  def funAspectSpecializationAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#AspectSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromAspectSubClassAxiom(ax)
    acc.copy(aspectSpecializationAxioms = acc.aspectSpecializationAxioms :+
      oml.tables.AspectSpecializationAxiom(
        uuid = info.uuid,
        tboxUUID = guuid,
        subEntityUUID = ops.getEntityUUID(info.sub),
        superAspectUUID = ops.getAspectUUID(info.sup)))
  }

  def funConceptSpecializationAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ConceptSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromConceptSpecializationAxiom(ax)
    acc.copy(conceptSpecializationAxioms = acc.conceptSpecializationAxioms :+
      oml.tables.ConceptSpecializationAxiom(
        uuid = info.uuid,
        tboxUUID = guuid,
        subConceptUUID = ops.getConceptUUID(info.sub),
        superConceptUUID = ops.getConceptUUID(info.sup)))
  }

  def funReifiedRelationshipSpecializationAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ReifiedRelationshipSpecializationAxiom)
  : Axioms
  = {
    val info = ops.fromReifiedRelationshipSpecializationAxiom(ax)
    acc.copy(reifiedRelationshipSpecializationAxioms = acc.reifiedRelationshipSpecializationAxioms :+
      oml.tables.ReifiedRelationshipSpecializationAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        subRelationshipUUID = ops.getReifiedRelationshipUUID(info.sub),
        superRelationshipUUID = ops.getReifiedRelationshipUUID(info.sup)))
  }

  def funSubDataPropertyOfAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#SubDataPropertyOfAxiom)
  : Axioms
  = {
    val info = ops.fromSubDataPropertyOfAxiom(ax)
    acc.copy(subDataPropertyOfAxioms = acc.subDataPropertyOfAxioms :+
      oml.tables.SubDataPropertyOfAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        subPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.sub),
        superPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.sup)))
  }

  def funSubObjectPropertyOfAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#SubObjectPropertyOfAxiom)
  : Axioms
  = {
    val info = ops.fromSubObjectPropertyOfAxiom(ax)
    acc.copy(subObjectPropertyOfAxioms = acc.subObjectPropertyOfAxioms :+
      oml.tables.SubObjectPropertyOfAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        subPropertyUUID = ops.getUnreifiedRelationshipUUID(info.sub),
        superPropertyUUID = ops.getUnreifiedRelationshipUUID(info.sup)))
  }

  def funEntityExistentialRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityExistentialRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityExistentialRestrictionAxiom(ax)
    acc.copy(entityExistentialRestrictionAxioms = acc.entityExistentialRestrictionAxioms :+
      oml.tables.EntityExistentialRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedDomainUUID = ops.getEntityUUID(info.domain),
        restrictedRangeUUID = ops.getEntityUUID(info.range),
        restrictedRelationshipUUID = ops.getRestrictableRelationshipUUID(info.restrictedRelationship)))
  }

  def funEntityUniversalRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityUniversalRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityUniversalRestrictionAxiom(ax)
    acc.copy(entityUniversalRestrictionAxioms = acc.entityUniversalRestrictionAxioms :+
      oml.tables.EntityUniversalRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedDomainUUID = ops.getEntityUUID(info.domain),
        restrictedRangeUUID = ops.getEntityUUID(info.range),
        restrictedRelationshipUUID = ops.getRestrictableRelationshipUUID(info.restrictedRelationship)))
  }

  def funEntityScalarDataPropertyExistentialRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyExistentialRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyExistentialRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyExistentialRestrictionAxioms = acc.entityScalarDataPropertyExistentialRestrictionAxioms :+
      oml.tables.EntityScalarDataPropertyExistentialRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedEntityUUID = ops.getEntityUUID(info.restrictedEntity),
        scalarPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.scalarDataProperty),
        scalarRestrictionUUID = ops.getDataRangeUUID(info.restrictedRange)))
  }

  def funEntityScalarDataPropertyParticularRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyParticularRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyParticularRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyParticularRestrictionAxioms = acc.entityScalarDataPropertyParticularRestrictionAxioms :+
      oml.tables.EntityScalarDataPropertyParticularRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedEntityUUID = ops.getEntityUUID(info.restrictedEntity),
        scalarPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.scalarDataProperty),
        literalValue = info.literalValue,
        valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) }))
  }

  def funEntityScalarDataPropertyUniversalRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityScalarDataPropertyUniversalRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityScalarDataPropertyUniversalRestrictionAxiom(ax)
    acc.copy(entityScalarDataPropertyUniversalRestrictionAxioms = acc.entityScalarDataPropertyUniversalRestrictionAxioms :+
      oml.tables.EntityScalarDataPropertyUniversalRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedEntityUUID = ops.getEntityUUID(info.restrictedEntity),
        scalarPropertyUUID = ops.getEntityScalarDataPropertyUUID(info.scalarDataProperty),
        scalarRestrictionUUID = ops.getDataRangeUUID(info.restrictedRange)))
  }

  def funEntityStructuredDataPropertyParticularRestrictionAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityStructuredDataPropertyParticularRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityStructuredDataPropertyParticularRestrictionAxiom(ax)
    acc.copy(entityStructuredDataPropertyParticularRestrictionAxioms = acc.entityStructuredDataPropertyParticularRestrictionAxioms :+
      oml.tables.EntityStructuredDataPropertyParticularRestrictionAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        restrictedEntityUUID = ops.getEntityUUID(info.restrictedEntity),
        structuredDataPropertyUUID = ops.getEntityStructuredDataPropertyUUID(info.structuredDataProperty)))
  }

  def funScalarOneOfLiteralAxiom[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#ScalarOneOfLiteralAxiom)
  : Axioms
  = {
    val info = ops.fromScalarOneOfLiteralAxiom(ax)
    acc.copy(scalarOneOfLiteralAxioms = acc.scalarOneOfLiteralAxioms :+
      oml.tables.ScalarOneOfLiteralAxiom(
        tboxUUID = guuid,
        uuid = info.uuid,
        axiomUUID = ops.getScalarOneOfRestrictionUUID(info.restriction),
        value = info.value,
        valueTypeUUID = info.valueType.map { vt => ops.getDataRangeUUID(vt) }))
  }

  def combine[omf <: OMF[omf]]
  (guuid: tables.taggedTypes.TerminologyBoxUUID, ops: OMFOps[omf])
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
    funSubDataPropertyOfAxiom =
      Axioms.funSubDataPropertyOfAxiom(guuid, ops, acc),
    funSubObjectPropertyOfAxiom =
      Axioms.funSubObjectPropertyOfAxiom(guuid, ops, acc),
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
    funEntityStructuredDataPropertyParticularRestrictionAxiom =
      Axioms.funEntityStructuredDataPropertyParticularRestrictionAxiom(guuid, ops, acc),
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

  implicit def TerminologyKindOrdering: Ordering[oml.tables.TerminologyKind] =
  new Ordering[oml.tables.TerminologyKind] {
    def compare(x: oml.tables.TerminologyKind, y: oml.tables.TerminologyKind): scala.Int
    = if (x == y) 0
    else if (x == oml.tables.OpenWorldDefinitions) -1
    else 1
  }

  def toTables[omf <: OMF[omf]]
  (ims: Seq[omf#ImmutableModule])
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ Seq[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)]
  = ims.foldLeft(Seq.empty[(omf#ImmutableModule, oml.tables.OMLSpecificationTables)].right[Throwables]) {
    case (acc, im) =>
      ops.foldImmutableModule(
        funImmutableTerminologyGraph = OMFTabularExportFromTerminologyGraph.toTables(acc),
        funImmutableTerminologyBundle = OMFTabularExportFromBundle.toTables(acc),
        funImmutableDescriptionBox = OMFTabularExportFromDescriptionBox.toTables(acc))(im)
  }


}
