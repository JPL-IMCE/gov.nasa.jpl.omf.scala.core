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
import gov.nasa.jpl.imce.omf.schema.tables._

import scala.collection.immutable.{Seq,Set}
import scala.{Option,None,Some}
import scala.Predef.{ArrowAssoc,String}

case class Axioms
( aspectSpecializationAxioms : Seq[AspectSpecializationAxiom] = Seq.empty,
  conceptSpecializationAxioms : Seq[ConceptSpecializationAxiom] = Seq.empty,
  reifiedRelationshipSpecializationAxioms : Seq[ReifiedRelationshipSpecializationAxiom] = Seq.empty,

  entityExistentialRestrictionAxioms : Seq[EntityExistentialRestrictionAxiom] = Seq.empty,
  entityUniversalRestrictionAxioms : Seq[EntityUniversalRestrictionAxiom] = Seq.empty,

  entityScalarDataPropertyExistentialRestrictionAxioms : Seq[EntityScalarDataPropertyExistentialRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyParticularRestrictionAxioms : Seq[EntityScalarDataPropertyParticularRestrictionAxiom] = Seq.empty,
  entityScalarDataPropertyUniversalRestrictionAxioms : Seq[EntityScalarDataPropertyUniversalRestrictionAxiom] = Seq.empty,

  scalarOneOfLiteralAxioms : Seq[ScalarOneOfLiteralAxiom] = Seq.empty )

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

  def funEntityDefinitionAspectSubClassAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityDefinitionAspectSubClassAxiom)
  : Axioms
  = {
    val info = ops.fromEntityDefinitionAspectSubClassAxiom(ax)
    acc.copy(aspectSpecializationAxioms = acc.aspectSpecializationAxioms :+
      AspectSpecializationAxiom(
        graphUUID = guuid,
        uuid = info._1.toString,
        subEntityUUID = ops.getTermUUID(info._2).toString,
        superAspectUUID = ops.getTermUUID(info._3).toString))
  }

  def funEntityConceptSubClassAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityConceptSubClassAxiom)
  : Axioms
  = {
    val info = ops.fromEntityConceptSubClassAxiom(ax)
    acc.copy(conceptSpecializationAxioms = acc.conceptSpecializationAxioms :+
      ConceptSpecializationAxiom(
        graphUUID = guuid,
        uuid = info._1.toString,
        subConceptUUID = ops.getTermUUID(info._2).toString,
        superConceptUUID = ops.getTermUUID(info._3).toString))
  }

  def funEntityReifiedRelationshipSubClassAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityReifiedRelationshipSubClassAxiom)
  : Axioms
  = {
    val info = ops.fromEntityReifiedRelationshipSubClassAxiom(ax)
    acc.copy(reifiedRelationshipSpecializationAxioms = acc.reifiedRelationshipSpecializationAxioms :+
      ReifiedRelationshipSpecializationAxiom(
        graphUUID = guuid,
        uuid = info._1.toString,
        subRelationshipUUID = ops.getTermUUID(info._2).toString,
        superRelationshipUUID = ops.getTermUUID(info._3).toString))
  }

  def funEntityDefinitionRestrictionAxiom[omf <: OMF]
  (guuid: String, ops: OMFOps[omf], acc: Axioms)
  (ax: omf#EntityDefinitionRestrictionAxiom)
  : Axioms
  = {
    val info = ops.fromEntityDefinitionRestrictionAxiom(ax)
    info._5 match {
      case ExistentialRestrictionKind =>
        acc.copy(entityExistentialRestrictionAxioms = acc.entityExistentialRestrictionAxioms :+
          EntityExistentialRestrictionAxiom(
            graphUUID = guuid,
            uuid = info._1.toString,
            restrictedDomainUUID = ops.getTermUUID(info._2).toString,
            restrictedRangeUUID = ops.getTermUUID(info._4).toString,
            restrictedRelationUUID = ops.getTermUUID(info._3).toString))
      case UniversalRestrictionKind =>
        acc.copy(entityUniversalRestrictionAxioms = acc.entityUniversalRestrictionAxioms :+
          EntityUniversalRestrictionAxiom(
            graphUUID = guuid,
            uuid = info._1.toString,
            restrictedDomainUUID = ops.getTermUUID(info._2).toString,
            restrictedRangeUUID = ops.getTermUUID(info._4).toString,
            restrictedRelationUUID = ops.getTermUUID(info._3).toString))
    }
  }

  def combine[omf <: OMF]
  (guuid: String, ops: OMFOps[omf])
  (acc: Axioms,
   ax: omf#ModelTermAxiom)
  : Axioms
  = ops.foldTermAxiom[Axioms](
    funEntityDefinitionAspectSubClassAxiom =
      Axioms.funEntityDefinitionAspectSubClassAxiom(guuid, ops, acc),
    funEntityConceptDesignationTerminologyGraphAxiom =
      (_: omf#EntityConceptDesignationTerminologyGraphAxiom) => acc,
    funEntityConceptSubClassAxiom =
      Axioms.funEntityConceptSubClassAxiom(guuid, ops, acc),
    funEntityDefinitionRestrictionAxiom =
      Axioms.funEntityDefinitionRestrictionAxiom(guuid, ops, acc),
    funEntityReifiedRelationshipSubClassAxiom =
      Axioms.funEntityReifiedRelationshipSubClassAxiom(guuid, ops, acc),
    funScalarDataTypeFacetRestrictionAxiom =
      (_: omf#ScalarDataTypeFacetRestrictionAxiom) => acc,
    funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =
      (_: omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral) => acc
  )(ax)
}

object OMFTabularExport {

  def toTables[omf <: OMF]
  (gs: Set[omf#ImmutableModelTerminologyGraph])
  (implicit store: omf#Store, ops: OMFOps[omf])
  : OMFSchemaTables
  = {
    val sigs = gs.map(g => g -> ops.fromTerminologyGraph(g))

    val allGraphs = sigs.map { case (_, sig) =>
      TerminologyGraph(
        uuid = sig.uuid.toString,
        kind = if (TerminologyKind.isDefinitionKind(sig.kind))
          OpenWorldDefinitions
        else
          ClosedWorldDesignations,
        name = sig.name,
        iri = sig.iri.toString)
    }.to[Seq]

    val allGraphExtensionAxioms = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.gaxioms.flatMap { gax =>
        ops.foldTerminologyGraphAxiom[Option[TerminologyExtensionAxiom]](
          funTerminologyGraphDirectExtensionAxiom =
            (gax: omf#TerminologyGraphDirectExtensionAxiom) =>
              Some(
                TerminologyExtensionAxiom(
                  uuid = ops.getTerminologyGraphAxiomUUID(gax).toString,
                  extendedTerminologyUUID = ops.getTerminologyGraphUUID(ops.getExtendedGraphOfTerminologogyGraphDirectExtensionAxiom(gax)).toString,
                  extendingTerminologyUUID = guuid)
              ),
          funTerminologyGraphDirectNestingAxiom =
            (_: omf#TerminologyGraphDirectNestingAxiom) =>
              None)(gax)
      }
    }.to[Seq]

    val allGraphNestingAxioms = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.gaxioms.flatMap { gax =>
        ops.foldTerminologyGraphAxiom[Option[TerminologyNestingAxiom]](
          funTerminologyGraphDirectExtensionAxiom =
            (_: omf#TerminologyGraphDirectExtensionAxiom) =>
              None,
          funTerminologyGraphDirectNestingAxiom =
            (gax: omf#TerminologyGraphDirectNestingAxiom) =>
              Some(
                TerminologyNestingAxiom(
                  uuid = ops.getTerminologyGraphAxiomUUID(gax).toString,
                  nestedTerminologyUUID = guuid,
                  nestingContextUUID = ops.getTermUUID(ops.getNestingContextConceptOfAxiom(gax)).toString,
                  nestingTerminologyUUID = ops.getTerminologyGraphUUID(ops.getNestingGraphOfAxiom(gax)).toString)
              ))(gax)
      }
    }.to[Seq]

    val allAspects = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.aspects.map { a =>
        Aspect(
          graphUUID = guuid,
          uuid = ops.getTermUUID(a).toString,
          name = ops.getTermLocalName(a),
          iri = ops.fromTerm(a).toString)
      }
    }.to[Seq]

    val allConcepts = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.concepts.map { c =>
        val sig = ops.fromEntityConcept(c)
        Concept(
          graphUUID = guuid,
          uuid = sig.uuid.toString,
          name = sig.name,
          iri = sig.iri.toString,
          isAbstract = sig.isAbstract)
      }
    }.to[Seq]

    val allReifiedRelationships = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.reifiedRelationships.map { rr =>
        val sig = ops.fromEntityReifiedRelationship(rr)
        ReifiedRelationship(
          graphUUID = guuid,
          uuid = sig.uuid.toString,
          isAbstract = sig.isAbstract,
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
    }.to[Seq]

    val allUnreifiedRelationships = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.unreifiedRelationships.map { ur =>
        val sig = ops.fromEntityUnreifiedRelationship(ur)
        UnreifiedRelationship(
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
    }.to[Seq]

    val allScalars = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.scalarDataTypes.map { sc =>
        Scalar(
          graphUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermLocalName(sc),
          iri = ops.fromTerm(sc).toString)
      }
    }.to[Seq]

    val allStructures = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structuredDataTypes.map { sc =>
        Structure(
          graphUUID = guuid,
          uuid = ops.getTermUUID(sc).toString,
          name = ops.getTermLocalName(sc),
          iri = ops.fromTerm(sc).toString)
      }
    }.to[Seq]

    val allEntity2ScalarProperties = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entity2scalarDataRelationships.map { e2sc =>
        val info = ops.fromDataRelationshipFromEntityToScalar(e2sc)
        EntityScalarDataProperty(
          graphUUID = guuid,
          uuid = info._1.toString,
          name = info._2,
          iri = info._3.toString,
          domainUUID = ops.getTermUUID(info._4).toString,
          rangeUUID = ops.getTermUUID(info._5).toString)
      }
    }.to[Seq]
    
    val allEntity2StructureProperties = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.entity2structureDataRelationships.map { e2sc =>
        val info = ops.fromDataRelationshipFromEntityToStructure(e2sc)
        EntityStructuredDataProperty(
          graphUUID = guuid,
          uuid = info._1.toString,
          name = info._2,
          iri = info._3.toString,
          domainUUID = ops.getTermUUID(info._4).toString,
          rangeUUID = ops.getTermUUID(info._5).toString)
      }
    }.to[Seq]

    val allScalarProperties = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structure2scalarDataRelationships.map { s2sc =>
        val info = ops.fromDataRelationshipFromStructureToScalar(s2sc)
        ScalarDataProperty(
          graphUUID = guuid,
          uuid = info._1.toString,
          name = info._2,
          iri = info._3.toString,
          domainUUID = ops.getTermUUID(info._4).toString,
          rangeUUID = ops.getTermUUID(info._5).toString)
      }
    }.to[Seq]

    val allStructuredProperties = sigs.flatMap { case (g, sig) =>
      val guuid = sig.uuid.toString
      sig.structure2structureDataRelationships.map { s2sc =>
        val info = ops.fromDataRelationshipFromStructureToStructure(s2sc)
        StructuredDataProperty(
          graphUUID = guuid,
          uuid = info._1.toString,
          name = info._2,
          iri = info._3.toString,
          domainUUID = ops.getTermUUID(info._4).toString,
          rangeUUID = ops.getTermUUID(info._5).toString)
      }
    }.to[Seq]

    val allAxioms = sigs.aggregate(Axioms())(
      seqop = {
        case (acc: Axioms, (g, sig)) =>
          val guuid = sig.uuid.toString
          val axioms = sig.axioms.aggregate(Axioms())(seqop=Axioms.combine(guuid, ops),combop=Axioms.append)
          Axioms.append(acc, axioms)
      },
      combop = Axioms.append)

    val t = OMFSchemaTables.createEmptyOMFSchemaTables().copy(
      // graphs
      terminologyGraphs =
        allGraphs,
      bundles =
        Seq.empty,

      // graph axioms
      conceptDesignationTerminologyGraphAxioms =
        Seq.empty,
      terminologyExtensionAxioms =
        allGraphExtensionAxioms,
      terminologyNestingAxioms =
        allGraphNestingAxioms,

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
        Seq.empty,
      iriScalarRestrictions =
        Seq.empty,
      numericScalarRestrictions =
        Seq.empty,
      plainLiteralScalarRestrictions =
        Seq.empty,
      scalarOneOfRestrictions =
        Seq.empty,
      stringScalarRestrictions =
        Seq.empty,
      timeScalarRestrictions =
        Seq.empty,

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
        allAxioms.aspectSpecializationAxioms,
      conceptSpecializationAxioms =
        allAxioms.conceptSpecializationAxioms,
      reifiedRelationshipSpecializationAxioms =
        allAxioms.reifiedRelationshipSpecializationAxioms,
      entityExistentialRestrictionAxioms =
        allAxioms.entityExistentialRestrictionAxioms,
      entityUniversalRestrictionAxioms =
        allAxioms.entityUniversalRestrictionAxioms,
      entityScalarDataPropertyExistentialRestrictionAxioms =
        allAxioms.entityScalarDataPropertyExistentialRestrictionAxioms,
      entityScalarDataPropertyParticularRestrictionAxioms =
        allAxioms.entityScalarDataPropertyParticularRestrictionAxioms,
      entityScalarDataPropertyUniversalRestrictionAxioms =
        allAxioms.entityScalarDataPropertyUniversalRestrictionAxioms,
      scalarOneOfLiteralAxioms =
        allAxioms.scalarOneOfLiteralAxioms
    )

    t
  }

}
