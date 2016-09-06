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
      result.isRight should equal(true)
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
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyGraphExtension(base, xsd._1)
        identifiedElement <- addEntityAspect(base, "IdentifiedElement")
        hasIdentifier = addDataRelationshipFromEntityToScalar(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataRelationshipName = "hasIdentifier")
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isDefinition)
        mission_extends_base <- addTerminologyGraphExtension(mission, base)
        component <- addEntityConcept(mission, "Component", isAbstract = false)
        component_extends_identifiedElement <- addEntityDefinitionAspectSubClassAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement)
        function <- addEntityConcept(mission, "Function", isAbstract = false)
        function_extends_identifiedElement <- addEntityDefinitionAspectSubClassAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement)
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

        message_extends_item <- addEntityConceptSubClassAxiom(mission, message, item)

        materialItem_extends_item <- addEntityConceptSubClassAxiom(mission, materialItem, item)

        baseSaved <- saveTerminologyGraph(base)

        missionSaved <- saveTerminologyGraph(mission)
      } yield {
        integer.isDefined should be(true)
        string.isDefined should be(true)

      }
    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd <- loadTerminologyGraph(xsd_iri)
        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupScalarDataType(xsd._1, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base <- loadTerminologyGraph(base_iri)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission <- loadTerminologyGraph(mission_iri)
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#hasIdentifier")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Function")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Component")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Performs")
      } yield {
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

        val string =
          lookupScalarDataType(base._1, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupEntityAspect(base._1, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

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

        val component =
          lookupEntityConcept(mission._1, component_iri, recursively = false)
        component.isDefined should be(true)

        val function =
          lookupEntityConcept(mission._1, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupEntityReifiedRelationship(mission._1, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info =
          fromEntityReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)
        component_performs_function_info.isAbstract should be(false)
      }
    }

  }

}