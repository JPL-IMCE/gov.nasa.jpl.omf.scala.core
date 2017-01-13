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
import scala.Some
import scala.{StringContext,Unit}
import scala.util.control.Exception._
import scalaz._, Scalaz._
import scala.collection.immutable.{List,Set}

abstract class OMFVocabularyMutabilityTest[omf <: OMF]
( val saveStore: omf#Store, saveOps: OMFOps[omf],
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
          message=s"withOMFSave failed: ${cause.getMessage}",
          cause=cause,
          failedCodeStackDepth=2)
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
            message=s"withOMFLoad failed: ${cause.getMessage}",
            cause=cause,
            failedCodeStackDepth=2)
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
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyExtension(base, xsd._1)
        identifiedElement <- addAspect(base, "IdentifiedElement")
        hasIdentifier = addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = "hasIdentifier")
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isDefinition)
        mission_extends_base <- addTerminologyExtension(mission, base)
        component <- addConcept(mission, "Component", isAbstract = false)
        component_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement)
        function <- addConcept(mission, "Function", isAbstract = false)
        function_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement)
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

        message_extends_item <- addConceptSpecializationAxiom(mission, message, item)

        materialItem_extends_item <- addConceptSpecializationAxiom(mission, materialItem, item)

        baseSaved <- saveTerminology(base)

        missionSaved <- saveTerminology(mission)
      } yield {
        integer.isDefined should be(true)
        string.isDefined should be(true)

        ()
      }
    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminology(xsd_iri)
        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd._1, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base <- loadTerminology(base_iri)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission <- loadTerminology(mission_iri)
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#hasIdentifier")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Function")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Component")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Performs")
      } yield {
        val s = ops.fromTerminology(base._1)
        s.imports.isEmpty should be(false)
        s.imports.toSet.contains(xsd._1) should be(true)
        s.aspects.isEmpty should be(false)
        s.concepts.isEmpty should be(true)
        s.reifiedRelationships.isEmpty should be(true)
        s.scalarDataTypes.isEmpty should be(true)
        s.structuredDataTypes.isEmpty should be(true)
        s.entityScalarDataProperties.isEmpty should be(false)
        s.entityStructuredDataProperties.isEmpty should be(true)
        s.scalarDataProperties.isEmpty should be(true)
        s.structuredDataProperties.isEmpty should be(true)
        s.axioms.isEmpty should be(true)

        val string =
          lookupDataRange(base._1, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupAspect(base._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

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

        val component =
          lookupConcept(mission._1, component_iri, recursively = false)
        component.isDefined should be(true)

        val function =
          lookupConcept(mission._1, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupReifiedRelationship(mission._1, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info =
          fromReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)
        component_performs_function_info.isAbstract should be(false)

        ()
      }
    }

  }

}