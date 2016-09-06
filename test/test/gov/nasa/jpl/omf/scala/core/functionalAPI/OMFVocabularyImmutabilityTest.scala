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

package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import org.scalatest._, exceptions._
import scala.{None, Some, StringContext, Unit}
import scala.util.control.Exception._
import scalaz._, Scalaz._
import scala.collection.immutable.{Iterable,List,Set}

abstract class OMFVocabularyImmutabilityTest[omf <: OMF]
(val saveStore: omf#Store, saveOps: OMFOps[omf],
 val loadStore: omf#Store, loadOps: OMFOps[omf]
) extends WordSpec with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave(testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit =

    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          throw new TestFailedException(
            message = s"withOMFSave failed: ${cause.getMessage}",
            cause = cause,
            failedCodeStackDepth = 2)
      }
      .apply({
        preOMFSave()
        val result = testCode(saveStore, saveOps)
        postOMFSave()
        val errors: Set[java.lang.Throwable] = result.swap.getOrElse(Set.empty)
        errors should be(Set.empty)
      })


  def preOMFLoad(): Unit

  def postOMFLoad(): Unit

  def withOMFLoad(testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit =

    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          throw new TestFailedException(
            message = s"withOMFLoad failed: ${cause.getMessage}",
            cause = cause,
            failedCodeStackDepth = 2)
      }
      .apply({
        preOMFLoad()
        val result = testCode(loadStore, loadOps)
        postOMFLoad()
        result.isRight should equal(true)
      })

  "vocabulary roundtrip test" when {

    "construct tboxes and save them" in withOMFSave { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminologyGraph(xsd_iri)

        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupScalarDataType(xsd._1, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        string = lookupScalarDataType(xsd._1, string_iri, recursively = false)

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        _ <- setTerminologyGraphShortName(base, Some("base"))
        _ <- setTerminologyGraphUUID(base, Some("UUID.base"))
        base_extends_xsd <- addTerminologyGraphExtension(base, xsd._1)

        identifiedElement <- addEntityAspect(base, "IdentifiedElement")
        _ <- setTermShortName(base, identifiedElement, Some("base:IdentifiedElement"))
        _ <- setTermUUID(base, identifiedElement, Some("UUID.base:IdentifiedElement"))
        hasIdentifier <- addDataRelationshipFromEntityToScalar(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataRelationshipName = "hasIdentifier")

        ibase <- asImmutableTerminologyGraph(base)

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isDefinition)
        _ <- setTerminologyGraphShortName(mission, Some("mission"))
        _ <- setTerminologyGraphUUID(mission, Some("UUID.mission"))
        mission_extends_ibase <- addTerminologyGraphExtension(mission, ibase._1)

        component <- addEntityConcept(mission, "Component", isAbstract = false)
        function <- addEntityConcept(mission, "Function", isAbstract = false)
        component_performs_function <- addEntityReifiedRelationship(
          graph = mission,
          source = component,
          target = function,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        item <- addEntityConcept(mission, "Item", isAbstract = false)
        message <- addEntityConcept(mission, "Message", isAbstract = false)
        materialItem <- addEntityConcept(mission, "MaterialItem", isAbstract = false)
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library <- makeTerminologyGraph(library_iri, isDefinition)
        _ <- setTerminologyGraphShortName(library, Some("library"))
        library_extends_mission <- addTerminologyGraphExtension(library, mission)

        starTracker <- addEntityConcept(library, "StarTracker", isAbstract=true)
        starTracker_isa_component <- addEntityConceptSubClassAxiom(library, starTracker, component)

        determinesAttitude <- addEntityConcept(library, "DeterminesAttitude", isAbstract=true)
        determinesAttitude_is_function <- addEntityConceptSubClassAxiom(library, determinesAttitude, function)

        determinesDeltaV <- addEntityConcept(library, "DeterminesDeltaV", isAbstract=true)
        determinesDeltaV_is_function <- addEntityConceptSubClassAxiom(library, determinesDeltaV, function)

        starTracker_performs_determinesAttitude <- addEntityReifiedRelationshipExistentialRestrictionAxiom(
          library, starTracker, component_performs_function, determinesAttitude)

        starTracker_determinesDeltaV_context <- addEntityReifiedRelationshipContextualizationAxiom(
          library, starTracker, component_performs_function, "determinesDeltaV", determinesDeltaV)

        starTracker_determinesAttitudeFast_context <- addEntityReifiedRelationshipContextualizationAxiom(
          library, starTracker, component_performs_function, "determinesAttitude (fast:coarse)", determinesAttitude)

        starTracker_determinesAttitudeSlow_context <- addEntityReifiedRelationshipContextualizationAxiom(
          library, starTracker, component_performs_function, "determinesAttitude (slow:precise)", determinesAttitude)

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system <- makeTerminologyGraph(system_iri, isDefinition)
        system_extends_library <- addTerminologyGraphExtension(system, library)

        s1 <- addEntityConcept(system, "S1", isAbstract=false)
        s1_is_starTracker <- addEntityConceptSubClassAxiom(system, s1, starTracker)
        s1_hasIdentifier <-
        addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(system, s1, hasIdentifier, "ST.primary")

        s2 <- addEntityConcept(system, "S2", isAbstract=false)
        s2_is_starTracker <- addEntityConceptSubClassAxiom(system, s2, starTracker)
        s2_hasIdentifier <-
        addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(system, s2, hasIdentifier, "ST.backup")

      } yield {

        val identifiedElement =
          lookupEntityAspect(ibase._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

        val mission_extends_base = addTerminologyGraphExtension(mission, ibase._1)
        mission_extends_base.isRight should equal(true)

        val component_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement.get)
        component_extends_identifiedElement.isRight should be(true)

        val function_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement.get)
        function_extends_identifiedElement.isRight should equal(true)

        val message_extends_item =
          addEntityConceptSubClassAxiom(mission, message, item)
        message_extends_item.isRight should equal(true)

        val materialItem_extends_item =
          addEntityConceptSubClassAxiom(mission, materialItem, item)
        materialItem_extends_item.isRight should equal(true)

        val baseSaved = saveTerminologyGraph(base)
        baseSaved.isRight should equal(true)

        val missionSaved = saveTerminologyGraph(mission)
        missionSaved.isRight should equal(true)

        val librarySaved = saveTerminologyGraph(library)
        librarySaved.isRight should equal(true)

        val systemSaved = saveTerminologyGraph(system)
        systemSaved.isRight should equal(true)
      }
    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminologyGraph(xsd_iri)
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base <- loadTerminologyGraph(base_iri)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission <- loadTerminologyGraph(mission_iri)
        integer_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")

        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#hasIdentifier")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Component")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Function")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Performs")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library <- loadTerminologyGraph(library_iri)
        starTracker_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#StarTracker")
        determinesAttitude_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#DeterminesAttitude")

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system <- loadTerminologyGraph(system_iri)
        s1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S1")
        s2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S2")

      } yield {

        {
          val s = ops.fromTerminologyGraph(base._1)
          s.imports.isEmpty should be(false)
          s.imports.toSet.contains(xsd._1) should be(true)
          s.aspects.isEmpty should be(false)
          s.concepts.isEmpty should be(true)
          s.reifiedRelationships.isEmpty should be(true)
          s.scalarDataTypes.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.entity2scalarDataRelationships.isEmpty should be(false)
          s.entity2structureDataRelationships.isEmpty should be(true)
          s.structure2scalarDataRelationships.isEmpty should be(true)
          s.structure2structureDataRelationships.isEmpty should be(true)
          s.axioms.isEmpty should be(true)
        }

        getTerminologyGraphShortName(base._1) should be(Some("base"))
        getTerminologyGraphUUID(base._1) should be(Some("UUID.base"))

        val integer = lookupScalarDataType(xsd._1, integer_iri, recursively = false)
        integer.isDefined should be(true)

        val string = lookupScalarDataType(base._1, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupEntityAspect(base._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)
        getTermShortName(base._1, identifiedElement.get) should be(Some("base:IdentifiedElement"))
        getTermShortUUID(base._1, identifiedElement.get) should be(Some("UUID.base:IdentifiedElement"))

        val hasIdentifier =
          lookupEntityDataRelationshipFromEntityToScalar(base._1, hasIdentifier_iri, recursively = false)
        hasIdentifier.isDefined should be(true)

        val (_, hasIdentifierSource, hasIdentifierTarget) =
          fromDataRelationshipFromEntityToScalar(hasIdentifier.get)
        identifiedElement.get should be(hasIdentifierSource)
        string.get should be(hasIdentifierTarget)

        {
          val s = ops.fromTerminologyGraph(mission._1)
          s.imports.isEmpty should be(false)
          s.imports.toSet.contains(base._1) should be(true)
          s.aspects.isEmpty should be(true)
          s.concepts.isEmpty should be(false)
          s.reifiedRelationships.isEmpty should be(false)
          s.scalarDataTypes.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.entity2scalarDataRelationships.isEmpty should be(true)
          s.entity2structureDataRelationships.isEmpty should be(true)
          s.structure2scalarDataRelationships.isEmpty should be(true)
          s.structure2structureDataRelationships.isEmpty should be(true)
          s.axioms.isEmpty should be(false)
        }

        getTerminologyGraphShortName(mission._1) should be(Some("mission"))
        getTerminologyGraphUUID(mission._1) should be(Some("UUID.mission"))

        val component = lookupEntityConcept(mission._1, component_iri, recursively = false)
        component.isDefined should be(true)

        val function = lookupEntityConcept(mission._1, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupEntityReifiedRelationship(mission._1, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info = fromEntityReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)
        component_performs_function_info.isAbstract should be(false)

        val starTracker = lookupEntityConcept(library._1, starTracker_iri, recursively = false)
        starTracker.isDefined should be(true)

        val determinesAttitude = lookupEntityConcept(library._1, determinesAttitude_iri, recursively = false)
        determinesAttitude.isDefined should be(true)

        val restrictions
        : Iterable[omf#EntityReifiedRelationshipRestrictionAxiom]
        = ops.getTermAxioms(library._1)._2.flatMap { ax =>
          ops.foldTermAxiom(ax)(
            funEntityDefinitionAspectSubClassAxiom =
              _ => None,
            funEntityConceptDesignationTerminologyGraphAxiom =
              _ => None,
            funEntityConceptSubClassAxiom =
              _ => None,
            funEntityDefinitionRestrictionAxiom =
              _ => None,
            funEntityReifiedRelationshipSubClassAxiom =
              _ => None,
            funEntityReifiedRelationshipContextualizationAxiom =
              _ => None,
            funEntityReifiedRelationshipRestrictionAxiom =
              (r: omf#EntityReifiedRelationshipRestrictionAxiom) => Some(r),
            funScalarDataTypeFacetRestrictionAxiom =
              _ => None,
            funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral = _ => None)
        }

        restrictions.exists { r =>
          ops.fromEntityReifiedRelationshipRestrictionAxiom(r) match {
            case (starTracker, component_performs_function, determinesAttitude, ExistentialRestrictionKind) =>
              true
            case _ =>
              false
          }
        } should be(true)

        val contextualizations
        : Iterable[omf#EntityReifiedRelationshipContextualizationAxiom]
        = ops.getTermAxioms(library._1)._2.flatMap { ax =>
          ops.foldTermAxiom(ax)(
            funEntityDefinitionAspectSubClassAxiom =
              _ => None,
            funEntityConceptDesignationTerminologyGraphAxiom =
              _ => None,
            funEntityConceptSubClassAxiom =
              _ => None,
            funEntityDefinitionRestrictionAxiom =
              _ => None,
            funEntityReifiedRelationshipSubClassAxiom =
              _ => None,
            funEntityReifiedRelationshipContextualizationAxiom =
              (r: omf#EntityReifiedRelationshipContextualizationAxiom) => Some(r),
            funEntityReifiedRelationshipRestrictionAxiom =
              _ => None,
            funScalarDataTypeFacetRestrictionAxiom =
              _ => None,
            funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral = _ => None)
        }

        // @todo The contextualization pattern needs to be recognized in the ImmutableModelTerminologyGraphResolver.

//        contextualizations.exists { r =>
//          ops.fromEntityReifiedRelationshipContextualizationAxiom(r) match {
//            case (starTracker, component_performs_function, "determinesAttitude_fastCoarse", determinesAttitude) =>
//              true
//            case _ =>
//              false
//          }
//        } should be(true)
//
//        contextualizations.exists { r =>
//          ops.fromEntityReifiedRelationshipContextualizationAxiom(r) match {
//            case (starTracker, component_performs_function, "determinesAttitude_slowPrecise", determinesAttitude) =>
//              true
//            case _ =>
//              false
//          }
//        } should be(true)

        val s1 = lookupEntityConcept(system._1, s1_iri, recursively=false)
        s1.isDefined should be(true)

        val s2 = lookupEntityConcept(system._1, s2_iri, recursively=false)
        s2.isDefined should be(true)

        val s1Restrictions = lookupEntityDefinitionScalarDataRelationshipRestrictions(system._1, s1.get)
        s1Restrictions.size should be(1)

        val s2Restrictions = lookupEntityDefinitionScalarDataRelationshipRestrictions(system._1, s2.get)
        s2Restrictions.size should be(1)
      }
    }
  }

}