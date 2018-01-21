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

import gov.nasa.jpl.imce.oml.tables.taggedTypes.localName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import org.scalatest._, exceptions._
import scala.Some
import scala.{StringContext,Unit}
import scala.util.control.Exception._
import scalaz._
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
        xsd_table <- loadTerminology(Mutable2ImmutableModuleTable.empty[omf], xsd_iri)
        (xsd, table1) = xsd_table

        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        string = lookupDataRange(xsd, string_iri, recursively = false)

        oml_iri <- makeIRI("http://imce.jpl.nasa.gov/oml/oml")
        oml_table <- loadTerminology(table1, oml_iri)
        (oml, table2) = oml_table

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isOpenWorld)
        base_extends_xsd <- addTerminologyExtension(base, oml)
        identifiedElement <- addAspect(base, localName("IdentifiedElement"))
        hasIdentifier = addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = localName("hasIdentifier"),
          isIdentityCriteria = false)
        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isOpenWorld)
        mission_extends_base <- addTerminologyExtension(mission, base)
        component <- addConcept(mission, localName("Component"))
        component_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement)
        function <- addConcept(mission, localName("Function"))
        function_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement)
        component_performs_function <- addReifiedRelationship(
          graph = mission,
          source = component,
          target = function,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = localName("Performs"),
          unreifiedRelationshipName = localName("performs"),
          unreifiedInverseRelationshipName = Some(localName("isPerformedBy")))

        performs = fromReifiedRelationship(component_performs_function).forwardProperty

        isPerformedBy = fromReifiedRelationship(component_performs_function).inverseProperty.get

        item <- addConcept(mission, localName("Item"))

        message <- addConcept(mission, localName("Message"))

        materialItem <- addConcept(mission, localName("MaterialItem"))

        message_extends_item <- addConceptSpecializationAxiom(mission, message, item)

        materialItem_extends_item <- addConceptSpecializationAxiom(mission, materialItem, item)

        omlSaved <- saveTerminology(oml)

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
        xsd_table <- loadTerminology(Mutable2ImmutableModuleTable.empty[omf], xsd_iri)
        (xsd, table1) = xsd_table

        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base")
        base_table <- loadTerminology(table1, base_iri)
        (base, table2) = base_table

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission")
        mission_table <- loadTerminology(table2, mission_iri)
        (mission, _) = mission_table

        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/base/base#hasIdentifier")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Function")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Component")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/mutability/foundation/mission/mission#Performs")
      } yield {
        val s = ops.fromImmutableTerminology(base)
        s.importedTerminologies.isEmpty should be(false)
        s.importedTerminologies.contains(xsd) should be(false)
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
          lookupDataRange(base, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupAspect(base, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

        val hasIdentifier =
          lookupEntityScalarDataProperty(base, hasIdentifier_iri, recursively = false)
        hasIdentifier.isDefined should be(true)

        val prop
        : EntityScalarDataPropertySignature[omf]
        = fromEntityScalarDataProperty(hasIdentifier.get)

        // TODO Investigate
        // Error:(230, 46) [Artima SuperSafe] Values of type omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } may not be compared for equality with ScalaTest's be matcher syntax. If you really want this expression to compile, configure Artima SuperSafe to allow omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } to be compared for equality.  For more information on this kind of error, see: http://www.artima.com/supersafe_user_guide.html#safer-equality
        // identifiedElement.get should be(prop.domain)

        prop.domain should be(identifiedElement.get)

        string.get should be(prop.range)

        {
          val s = ops.fromImmutableTerminology(mission)
          s.importedTerminologies.isEmpty should be(false)
          s.importedTerminologies.contains(base) should be(true)
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
          lookupConcept(mission, component_iri, recursively = false)
        component.isDefined should be(true)

        val function =
          lookupConcept(mission, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupReifiedRelationship(mission, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info =
          fromReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)

        ()
      }
    }

  }

}