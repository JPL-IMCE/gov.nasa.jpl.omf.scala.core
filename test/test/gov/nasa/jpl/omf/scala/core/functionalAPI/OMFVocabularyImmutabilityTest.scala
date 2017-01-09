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

import java.util.UUID

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import org.scalatest._, exceptions._
import scala.{Option, None, Some, StringContext, Unit}
import scala.util.control.Exception._
import scalaz._, Scalaz._
import scala.collection.immutable.{List,Set}

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
        result.leftMap { errors =>
          throw new TestFailedException(
            message=Some(s"withOMFSave ${errors.size} errors"),
            cause=errors.headOption,
            failedCodeStackDepth = 1
          )
        }
        ()
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
        result.leftMap { errors =>
          throw new TestFailedException(
            message=Some(s"withOMFLoad ${errors.size} errors"),
            cause=errors.headOption,
            failedCodeStackDepth = 1
          )
        }
        ()
      })

  "vocabulary roundtrip test" when {

    "construct tboxes and save them" in withOMFSave { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminology(xsd_iri)

        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd._1, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        string = lookupDataRange(xsd._1, string_iri, recursively = false)

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyExtension(base, xsd._1)

        identifiedElement <- addAspect(base, "IdentifiedElement")
        hasIdentifier <- addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = "hasIdentifier")

        ibase <- asImmutableTerminology(base)

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isDefinition)
        mission_extends_ibase <- addTerminologyExtension(mission, ibase._1)

        component <- addConcept(mission, "Component", isAbstract = false)
        function <- addConcept(mission, "Function", isAbstract = false)
        component_performs_function <- addReifiedRelationship(
          graph = mission,
          source = component,
          target = function,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        item <- addConcept(mission, "Item", isAbstract = false)
        message <- addConcept(mission, "Message", isAbstract = false)
        materialItem <- addConcept(mission, "MaterialItem", isAbstract = false)
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library <- makeTerminologyGraph(library_iri, isDefinition)
        library_extends_mission <- addTerminologyExtension(library, mission)

        starTracker <- addConcept(library, "StarTracker", isAbstract=true)
        starTracker_isa_component <- addConceptSpecializationAxiom(library, starTracker, component)

        determinesAttitude <- addConcept(library, "DeterminesAttitude", isAbstract=true)
        determinesAttitude_is_function <- addConceptSpecializationAxiom(library, determinesAttitude, function)

        determinesDeltaV <- addConcept(library, "DeterminesDeltaV", isAbstract=true)
        determinesDeltaV_is_function <- addConceptSpecializationAxiom(library, determinesDeltaV, function)

        starTracker_performs_determinesAttitude <- addEntityExistentialRestrictionAxiom(
          library, starTracker, component_performs_function, determinesAttitude)

        starTracker_determinesDeltaV_context <- addEntityExistentialRestrictionAxiom(
          library, starTracker, component_performs_function, determinesDeltaV)

        starTracker_determinesAttitudeFast_context <- addEntityExistentialRestrictionAxiom(
          library, starTracker, component_performs_function, determinesAttitude)

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system <- makeTerminologyGraph(system_iri, isDefinition)
        system_extends_library <- addTerminologyExtension(system, library)

        s1 <- addConcept(system, "S1", isAbstract=false)
        s1_is_starTracker <- addConceptSpecializationAxiom(system, s1, starTracker)
        s1_hasIdentifier <- addEntityScalarDataPropertyParticularRestrictionAxiom(system, s1, hasIdentifier, "ST.primary")

        s2 <- addConcept(system, "S2", isAbstract=false)
        s2_is_starTracker <- addConceptSpecializationAxiom(system, s2, starTracker)
        s2_hasIdentifier <- addEntityScalarDataPropertyParticularRestrictionAxiom(system, s2, hasIdentifier, "ST.backup")

      } yield {

        val identifiedElement =
          lookupAspect(ibase._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

        val mission_extends_base = addTerminologyExtension(mission, ibase._1)
        mission_extends_base.isRight should equal(false)

        val component_extends_identifiedElement = addAspectSpecializationAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement.get)
        component_extends_identifiedElement.isRight should be(true)

        val function_extends_identifiedElement = addAspectSpecializationAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement.get)
        function_extends_identifiedElement.isRight should equal(true)

        val message_extends_item =
          addConceptSpecializationAxiom(mission, message, item)
        message_extends_item.isRight should equal(true)

        val materialItem_extends_item =
          addConceptSpecializationAxiom(mission, materialItem, item)
        materialItem_extends_item.isRight should equal(true)

        val baseSaved = saveTerminology(base)
        baseSaved.isRight should equal(true)

        val missionSaved = saveTerminology(mission)
        missionSaved.isRight should equal(true)

        val librarySaved = saveTerminology(library)
        librarySaved.isRight should equal(true)

        val systemSaved = saveTerminology(system)
        systemSaved.isRight should equal(true)
      }
    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminology(xsd_iri)
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base <- loadTerminology(base_iri)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission <- loadTerminology(mission_iri)
        integer_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")

        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#hasIdentifier")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Component")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Function")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Performs")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library <- loadTerminology(library_iri)
        starTracker_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#StarTracker")
        determinesAttitude_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#DeterminesAttitude")

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system <- loadTerminology(system_iri)
        s1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S1")
        s2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S2")

      } yield {

        {
          val s = ops.fromTerminology(base._1)
          s.imports.isEmpty should be(false)
          s.imports.toSet.contains(xsd._1) should be(true)
          s.aspects.isEmpty should be(false)
          s.concepts.isEmpty should be(true)
          s.reifiedRelationships.isEmpty should be(true)
          s.scalarDataTypes.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.entityScalarDataProperties.nonEmpty should be(true)
          s.entityStructuredDataProperties.isEmpty should be(true)
          s.scalarDataProperties.isEmpty should be(true)
          s.structuredDataProperties.isEmpty should be(true)
          s.axioms.isEmpty should be(true)
        }

        getTerminologyName(base._1) should be("base")
        getTerminologyUUID(base._1) should be(UUID.fromString("73468cd7-d400-5fa1-b460-a15aeb8f64b6"))

        val integer = lookupDataRange(xsd._1, integer_iri, recursively = false)
        integer.isDefined should be(true)

        val string = lookupDataRange(base._1, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupAspect(base._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)
        getTermName(identifiedElement.get) should be("IdentifiedElement")
        getTermUUID(identifiedElement.get) should be(UUID.fromString("06e317f7-755c-5ed9-a5b6-7c105f687d59"))
        generateUUID("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement").toString should be("06e317f7-755c-5ed9-a5b6-7c105f687d59")

        val hasIdentifier =
          lookupEntityScalarDataProperty(base._1, hasIdentifier_iri, recursively = false)
        hasIdentifier.isDefined should be(true)

        val prop = fromEntityScalarDataProperty(hasIdentifier.get)
        identifiedElement.get should be(prop.domain)
        string.get should be(prop.range)

        {
          val s = ops.fromTerminology(mission._1)
          s.imports.isEmpty should be(false)
          s.imports.toSet.contains(base._1) should be(true)
          s.aspects.isEmpty should be(true)
          s.concepts.isEmpty should be(false)
          s.reifiedRelationships.isEmpty should be(false)
          s.scalarDataTypes.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.entityScalarDataProperties.isEmpty should be(true)
          s.entityStructuredDataProperties.isEmpty should be(true)
          s.scalarDataProperties.isEmpty should be(true)
          s.structuredDataProperties.isEmpty should be(true)
          s.axioms.isEmpty should be(false)
        }

        getTerminologyName(mission._1) should be("mission")
        getTerminologyUUID(mission._1) should be(UUID.fromString("5551c7f4-1210-5c4b-bd2b-976625a971e4"))

        val component = lookupConcept(mission._1, component_iri, recursively = false)
        component.isDefined should be(true)

        val function = lookupConcept(mission._1, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupReifiedRelationship(mission._1, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info = fromReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)
        component_performs_function_info.isAbstract should be(false)

        val starTracker = lookupConcept(library._1, starTracker_iri, recursively = false)
        starTracker.isDefined should be(true)

        val determinesAttitude = lookupConcept(library._1, determinesAttitude_iri, recursively = false)
        determinesAttitude.isDefined should be(true)

        val s1 = lookupConcept(system._1, s1_iri, recursively=false)
        s1.isDefined should be(true)

        val s2 = lookupConcept(system._1, s2_iri, recursively=false)
        s2.isDefined should be(true)

        val s1Restrictions = fromTerminology(system._1).axioms.flatMap { ax =>
          foldAxiom[Option[omf#EntityScalarDataPropertyParticularRestrictionAxiom]](
            funAspectSpecializationAxiom = _ => None,
            funConceptSpecializationAxiom = _ => None,
            funReifiedRelationshipSpecializationAxiom = _ => None,
            funEntityExistentialRestrictionAxiom = _ => None,
            funEntityUniversalRestrictionAxiom = _ => None,
            funEntityScalarDataPropertyExistentialRestrictionAxiom = _ => None,
            funEntityScalarDataPropertyParticularRestrictionAxiom =
            (x: omf#EntityScalarDataPropertyParticularRestrictionAxiom) =>
              if (fromEntityScalarDataPropertyParticularRestrictionAxiom(x).restrictedEntity == s1.get)
                Some(x)
              else
                None,
            funEntityScalarDataPropertyUniversalRestrictionAxiom = _ => None,
            funScalarOneOfLiteralAxiom = _ => None
          )(ax)
        }
        s1Restrictions.size should be(1)

        val s2Restrictions = fromTerminology(system._1).axioms.flatMap { ax =>
          foldAxiom[Option[omf#EntityScalarDataPropertyParticularRestrictionAxiom]](
            funAspectSpecializationAxiom = _ => None,
            funConceptSpecializationAxiom = _ => None,
            funReifiedRelationshipSpecializationAxiom = _ => None,
            funEntityExistentialRestrictionAxiom = _ => None,
            funEntityUniversalRestrictionAxiom = _ => None,
            funEntityScalarDataPropertyExistentialRestrictionAxiom = _ => None,
            funEntityScalarDataPropertyParticularRestrictionAxiom =
              (x: omf#EntityScalarDataPropertyParticularRestrictionAxiom) =>
                if (fromEntityScalarDataPropertyParticularRestrictionAxiom(x).restrictedEntity == s2.get)
                  Some(x)
                else
                  None,
            funEntityScalarDataPropertyUniversalRestrictionAxiom = _ => None,
            funScalarOneOfLiteralAxiom = _ => None
          )(ax)
        }
        s2Restrictions.size should be(1)
      }
    }
  }

}