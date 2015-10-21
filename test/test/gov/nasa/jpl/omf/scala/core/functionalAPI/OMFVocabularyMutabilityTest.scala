/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.language.{implicitConversions, postfixOps}
import org.scalatest._, exceptions._
import scala.{StringContext,Unit}
import scala.util.control.Exception._
import scalaz._, Scalaz._
import scala.collection.immutable.List

abstract class OMFVocabularyMutabilityTest[omf <: OMF]
( val saveStore: omf#Store, saveOps: OMFOps[omf],
  val loadStore: omf#Store, loadOps: OMFOps[omf]
) extends WordSpec with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave(testCode: (omf#Store, OMFOps[omf]) => NonEmptyList[java.lang.Throwable] \/ Unit)
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

  def withOMFLoad(testCode: (omf#Store, OMFOps[omf]) => NonEmptyList[java.lang.Throwable] \/ Unit)
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
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyGraphExtension(base, xsd._1)
        identifiedElement <- addEntityAspect(base, "IdentifiedElement")
        hasIdentifier = addDataRelationshipFromEntityToScalar(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataRelationshipName = "hasIdentifier")
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission")
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
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/base/base")
        base <- loadTerminologyGraph(base_iri)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission")
        mission <- loadTerminologyGraph(mission_iri)
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/base/base#hasIdentifier")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Component")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Function")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Performs")
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
