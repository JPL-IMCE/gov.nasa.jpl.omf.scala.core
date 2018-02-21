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
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.scalatest._
import exceptions._

import scala.collection.immutable.{List, Set}
import scala.util.control.Exception._
import scala.{None, Option, Some, StringContext, Unit}
import scala.Predef.ArrowAssoc
import scalaz._

abstract class OMFVocabularyImmutabilityTest[omf <: OMF[omf]]
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
        drc <- loadBuiltinDatatypeMap()
        om <- initializeOntologyMapping(drc)
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd_table <- loadTerminology(om, xsd_iri)
        (xsd, table1) = xsd_table

        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        string = lookupDataRange(xsd, string_iri, recursively = false)

        oml_iri <- makeIRI("http://imce.jpl.nasa.gov/oml/oml")
        oml_table <- loadTerminology(table1, oml_iri)
        (oml, table2) = oml_table

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isOpenWorld)
        base_extends_xsd <- addTerminologyExtension(base, oml)

        identifiedElement <- addAspect(base, localName("IdentifiedElement"))
        hasIdentifier <- addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = localName("hasIdentifier"),
          isIdentityCriteria = false)

        m2i_base <- asImmutableTerminologyGraph(base, table2)
        (ibase, table2) = m2i_base

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isOpenWorld)
        mission_extends_ibase <- addTerminologyExtension(mission, ibase)

        component <- addConcept(mission, localName("Component"))
        function <- addConcept(mission, localName("Function"))
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
        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library <- makeTerminologyGraph(library_iri, isOpenWorld)
        library_extends_mission <- addTerminologyExtension(library, mission)

        starTracker <- addConcept(library, localName("StarTracker"))
        starTracker_isa_component <- addConceptSpecializationAxiom(library, starTracker, component)

        determinesAttitude <- addConcept(library, localName("DeterminesAttitude"))
        determinesAttitude_is_function <- addConceptSpecializationAxiom(library, determinesAttitude, function)

        determinesDeltaV <- addConcept(library, localName("DeterminesDeltaV"))
        determinesDeltaV_is_function <- addConceptSpecializationAxiom(library, determinesDeltaV, function)

        starTracker_performs_determinesAttitude <- addEntityExistentialRestrictionAxiom(
          library, starTracker, performs, determinesAttitude)

        starTracker_determinesDeltaV_context <- addEntityExistentialRestrictionAxiom(
          library, starTracker, performs, determinesDeltaV)

        starTracker_determinesAttitudeFast_context <- addEntityExistentialRestrictionAxiom(
          library, starTracker, performs, determinesAttitude)

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system <- makeTerminologyGraph(system_iri, isOpenWorld)
        system_extends_library <- addTerminologyExtension(system, library)

        s1 <- addConcept(system, localName("S1"))
        s1_is_starTracker <- addConceptSpecializationAxiom(system, s1, starTracker)
        s1_hasIdentifier <- addEntityScalarDataPropertyParticularRestrictionAxiom(
          system, s1, hasIdentifier,
          tables.LiteralValue(tables.LiteralStringType, "ST.primary"), string)

        s2 <- addConcept(system, localName("S2"))
        s2_is_starTracker <- addConceptSpecializationAxiom(system, s2, starTracker)
        s2_hasIdentifier <- addEntityScalarDataPropertyParticularRestrictionAxiom(
          system, s2, hasIdentifier,
          tables.LiteralValue(tables.LiteralStringType, "ST.backup"), string)

      } yield {

        val identifiedElement =
          lookupAspect(ibase, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)

        val mission_extends_base = addTerminologyExtension(mission, ibase)
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

        ()
      }
    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        drc <- loadBuiltinDatatypeMap()
        om <- initializeOntologyMapping(drc)
        xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
        xsd_table <- loadTerminology(om, xsd_iri)
        (xsd, table1) = xsd_table

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base")
        base_table <- loadTerminology(table1, base_iri)
        (base, table2) = base_table

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission")
        mission_table <- loadTerminology(table2, mission_iri)
        (mission, table3) = mission_table

        integer_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")

        identifiedElement_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#IdentifiedElement")
        hasIdentifier_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/base/base#hasIdentifier")
        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Component")
        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Function")
        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/foundation/mission/mission#Performs")

        library_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library")
        library_table <- loadTerminology(table3, library_iri)
        (library, table4) = library_table

        starTracker_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#StarTracker")
        determinesAttitude_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/library#DeterminesAttitude")

        system_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system")
        system_table <- loadTerminology(table4, system_iri)
        (system, _) = system_table

        s1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S1")
        s2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/immutability/system#S2")

      } yield {

        {
          val s = ops.fromImmutableTerminology(base)
          s.importedTerminologies.isEmpty should be(false)
          s.importedTerminologies.contains(xsd_iri) should be(false)
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

        getModuleName(base) should be("base")
        getModuleUUID(base).toString should be("73468cd7-d400-5fa1-b460-a15aeb8f64b6")

        val integer = lookupDataRange(xsd, integer_iri, recursively = false)
        integer.isDefined should be(true)

        val string = lookupDataRange(base, string_iri, recursively = true)
        string.isDefined should be(true)

        val identifiedElement = lookupAspect(base, identifiedElement_iri, recursively = false)
        identifiedElement.isDefined should be(true)
        getTermName(identifiedElement.get) should be("IdentifiedElement")
        getTermUUID(identifiedElement.get).toString should be("e95bcdd1-d88c-5d2c-86e0-6eb42c968570")

        val identifiedElementUUID = generateUUIDFromString(getModuleUUID(base), "name" -> localName("IdentifiedElement"))
        identifiedElementUUID.toString should be("e95bcdd1-d88c-5d2c-86e0-6eb42c968570")

        val hasIdentifier =
          lookupEntityScalarDataProperty(base, hasIdentifier_iri, recursively = false)
        hasIdentifier.isDefined should be(true)

        val prop = fromEntityScalarDataProperty(hasIdentifier.get)

        fromEntity(identifiedElement.get).uuid should be(fromEntity(prop.domain).uuid)
        fromEntity(identifiedElement.get) should be(fromEntity(prop.domain))

        // TODO Investigate...
        // Error:(324, 46) [Artima SuperSafe] Values of type omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } may not be compared for equality with ScalaTest's be matcher syntax. If you really want this expression to compile, configure Artima SuperSafe to allow omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } to be compared for equality.  For more information on this kind of error, see: http://www.artima.com/supersafe_user_guide.html#safer-equality
        //identifiedElement.get should be(prop.domain)

        val _a: omf#Aspect = identifiedElement.get
        val _e: omf#Entity = prop.domain

        // TODO Investigate...
        // Error:(331, 22) [Artima SuperSafe] Values of type omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } may not be compared for equality with ScalaTest's be matcher syntax. If you really want this expression to compile, configure Artima SuperSafe to allow omf#Entity and _38.Term with _38.Predicate forSome { val _38: omf } to be compared for equality.  For more information on this kind of error, see: http://www.artima.com/supersafe_user_guide.html#safer-equality
        //_a should be(_e)
        _e should be(_a)

        string.get should be(prop.range)

        {
          val s = ops.fromImmutableTerminology(mission)
          s.importedTerminologies.isEmpty should be(false)
          s.importedTerminologies.contains(base_iri) should be(true)
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

        getModuleName(mission) should be("mission")
        getModuleUUID(mission).toString should be("5551c7f4-1210-5c4b-bd2b-976625a971e4")

        val component = lookupConcept(mission, component_iri, recursively = false)
        component.isDefined should be(true)

        val function = lookupConcept(mission, function_iri, recursively = false)
        function.isDefined should be(true)

        val component_performs_function =
          lookupReifiedRelationship(mission, component_performs_function_iri, recursively = false)
        component_performs_function.isDefined should be(true)

        val component_performs_function_info = fromReifiedRelationship(component_performs_function.get)
        component_performs_function_info.source should be(component.get)
        component_performs_function_info.target should be(function.get)

        val starTracker = lookupConcept(library, starTracker_iri, recursively = false)
        starTracker.isDefined should be(true)

        val determinesAttitude = lookupConcept(library, determinesAttitude_iri, recursively = false)
        determinesAttitude.isDefined should be(true)

        val s1 = lookupConcept(system, s1_iri, recursively=false)
        s1.isDefined should be(true)

        val s2 = lookupConcept(system, s2_iri, recursively=false)
        s2.isDefined should be(true)

        val s1Restrictions = fromImmutableTerminology(system).axioms.flatMap { ax =>
          foldAxiom[Option[omf#EntityScalarDataPropertyParticularRestrictionAxiom]](
            funAspectSpecializationAxiom = _ => None,
            funConceptSpecializationAxiom = _ => None,
            funReifiedRelationshipSpecializationAxiom = _ => None,
            funSubDataPropertyOfAxiom = _ => None,
            funSubObjectPropertyOfAxiom = _ => None,
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
            funEntityStructuredDataPropertyParticularRestrictionAxiom = _ => None,
            funScalarOneOfLiteralAxiom = _ => None
          )(ax)
        }
        s1Restrictions.size should be(1)

        val s2Restrictions = fromImmutableTerminology(system).axioms.flatMap { ax =>
          foldAxiom[Option[omf#EntityScalarDataPropertyParticularRestrictionAxiom]](
            funAspectSpecializationAxiom = _ => None,
            funConceptSpecializationAxiom = _ => None,
            funReifiedRelationshipSpecializationAxiom = _ => None,
            funSubDataPropertyOfAxiom = _ => None,
            funSubObjectPropertyOfAxiom = _ => None,
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
            funEntityStructuredDataPropertyParticularRestrictionAxiom = _ => None,
            funScalarOneOfLiteralAxiom = _ => None
          )(ax)
        }
        s2Restrictions.size should be(1)

        ()
      }
    }
  }

}