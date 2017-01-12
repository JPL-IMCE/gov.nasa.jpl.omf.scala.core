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

import java.io.File
import gov.nasa.jpl.imce.omf.schema.tables.OMFSchemaTables
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import exceptions._
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport

import scala.{Option, Some, StringContext, Unit}
import scala.Predef.String
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import scala.collection.immutable.{List, Set}

abstract class OMFNestedGraphTest[omf <: OMF]
( testName: String,
  var createdTables: Option[File],
  var loadedTables: Option[File],
  val saveStore: omf#Store, saveOps: OMFOps[omf],
  val loadStore: omf#Store, loadOps: OMFOps[omf]
) extends WordSpec with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave
  (testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit
  = nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          throw new TestFailedException(
            message=s"withOMFSave failed: ${cause.getMessage}",
            cause=cause,
            failedCodeStackDepth=3)
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

  def withOMFLoad
  (testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit
  = nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          throw new TestFailedException(
            message=s"withOMFLoad failed: ${cause.getMessage}",
            cause=cause,
            failedCodeStackDepth=3)
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

  "nested graph roundtrip test" when {

    "construct nested tboxes and save them" in withOMFSave { (s, o) =>

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

        base_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyExtension(base, xsd._1)
        identifiedElement <- addAspect(base, "IdentifiedElement")
        hasIdentifier = addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = "hasIdentifier")

        mission_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission")
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

        project_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project")
        project <- makeTerminologyGraph(project_iri, isDefinition)
        project_extends_mission <- addTerminologyExtension(project, mission)
        workPackage <- addConcept(project, "WorkPackage", isAbstract = false)

        g_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G")
        g <- makeTerminologyGraph(g_iri, isDefinition)
        g_extends_project <- addTerminologyExtension(g, project)

        g_A <- addConcept(g, "A", isAbstract=false)
        g_A_isa_component <- addConceptSpecializationAxiom(g, g_A, component)

        g_B <- addConcept(g, "B", isAbstract=false)
        g_B_isa_component <- addConceptSpecializationAxiom(g, g_B, component)

        g_C <- addConcept(g, "C", isAbstract=false)
        g_C_isa_function <- addConceptSpecializationAxiom(g, g_C, component)

        p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P1")
        p1 <- makeTerminologyGraph(p1_iri, isDefinition)
        _ <- addTerminologyExtension(p1, g)
        g_authorizes_p1 <- addConcept(g, "P1", isAbstract=false)
        g_authorizes_p1_WP <- addConceptSpecializationAxiom(g, g_authorizes_p1, workPackage)

        p1_asserts_A_performs_C <- addReifiedRelationship(
          graph = p1,
          source = g_A,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        p1_asserts_A_performsFunction_C <-
        addReifiedRelationshipSpecializationAxiom(graph=p1, sub=p1_asserts_A_performs_C, sup=component_performs_function)

        g_nests_p1 <- addNestedTerminology(nestingGraph=g, nestingContext=g_authorizes_p1, nestedGraph=p1)

        p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P2")
        p2 <- makeTerminologyGraph(p2_iri, isDefinition)
        _ <- addTerminologyExtension(p2, g)
        g_authorizes_p2 <- addConcept(g, "P2", isAbstract=false)
        g_authorizes_p2_WP <- addConceptSpecializationAxiom(g, g_authorizes_p2, workPackage)

        p2_asserts_B_performs_C <- addReifiedRelationship(
          graph = p2,
          source = g_B,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        p2_asserts_B_performsFunction_C <-
        addReifiedRelationshipSpecializationAxiom(graph=p2, sub=p2_asserts_B_performs_C, sup=component_performs_function)

        g_nests_p2 <- addNestedTerminology(nestingGraph=g, nestingContext=g_authorizes_p2, nestedGraph=p2)

        ibase <- asImmutableTerminology(base)
        _ <- saveTerminology(ibase._1)

        imission <- asImmutableTerminology(ibase._2, mission)
        _ <- saveTerminology(imission._1)

        iproject <- asImmutableTerminology(imission._2, project)
        _ <- saveTerminology(iproject._1)

        ig <- asImmutableTerminology(iproject._2, g)
        _ <- saveTerminology(ig._1)

        ip1 <- asImmutableTerminology(ig._2, p1)
        _ <- saveTerminology(ip1._1)

        ip2 <- asImmutableTerminology(ig._2, p2)
        _ <- saveTerminology(ip2._1)

        tables = OMFTabularExport.toTables(
          Set[omf#ImmutableTerminologyBox](
            ibase._1, imission._1, iproject._1, ig._1, ip1._1, ip2._1))

        tablesIRI <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/Constructed/tables.json.zip")

        tablesFile <- resolveIRIAsLocalFile(tablesIRI)

        _ = java.nio.file.Files.createDirectories(tablesFile.getParentFile.toPath)

        _ <- OMFSchemaTables
          .saveOMFSchemaTables(tables, tablesFile)
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))

        _ = createdTables = Some(tablesFile)

      } yield {
        lookupNestingAxiomForNestedChildIfAny(nestedG = g).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ig._1).isEmpty should be(true)

        lookupNestingAxiomForNestedChildIfAny(nestedG = p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip1._1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p1)
        }

        lookupNestingAxiomForNestedChildIfAny(nestedG = p2).contains(g_nests_p2) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip2._1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p2)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2).contains(g_nests_p2) should be(true)
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

        base_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/base/base")
        base <- loadTerminology(base_iri)

        mission_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission")
        mission <- loadTerminology(mission_iri)

        component_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Component")
        component = lookupConcept(mission._1, component_iri, recursively=false)

        function_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Function")
        function = lookupConcept(mission._1, function_iri, recursively=false)

        component_performs_function_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Performs")

        project_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project")
        project <- loadTerminology(project_iri)

        workPackage_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project#WorkPackage")

        g_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G")
        g <- loadTerminology(g_iri)

        a_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#A")
        a = lookupConcept(g._1, a_iri, recursively=false)

        b_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#B")
        b = lookupConcept(g._1, b_iri, recursively=false)

        c_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#C")
        c = lookupConcept(g._1, c_iri, recursively=false)

        p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P1")
        p1 <- loadTerminology(p1_iri)

        g_authorizes_p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#P1")
        g_authorizes_p1 = lookupConcept(g._1, g_authorizes_p1_iri, recursively=false)

        p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P2")
        p2 <- loadTerminology(p2_iri)

        g_authorizes_p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#P2")
        g_authorizes_p2 = lookupConcept(g._1, g_authorizes_p2_iri, recursively=false)


        tables = OMFTabularExport.toTables(
          Set[omf#ImmutableTerminologyBox](
            base._1, mission._1, project._1, g._1, p1._1, p2._1))

        tablesIRI <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/Loaded/tables.json.zip")

        tablesFile <- resolveIRIAsLocalFile(tablesIRI)

        _ = java.nio.file.Files.createDirectories(tablesFile.getParentFile.toPath)

        _ <- OMFSchemaTables
          .saveOMFSchemaTables(tables, tablesFile)
          .toDisjunction
          .leftMap(Set[java.lang.Throwable](_))

        _ = loadedTables = Some(tablesFile)

      } yield {
        a.isDefined should be(true)
        b.isDefined should be(true)
        c.isDefined should be(true)
        g_authorizes_p1.isDefined should be(true)
        g_authorizes_p2.isDefined should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1._1).nonEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = g._1).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1._1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p1.get)
        }
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2._1).nonEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2._1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p2.get)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1.get).nonEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2.get).nonEmpty should be(true)
      }

    }

    "compare created vs loaded" in {
      import scala.sys.process._

      createdTables.nonEmpty should be(true)
      loadedTables.nonEmpty should be(true)
      java.lang.System.out.println(s"$testName.Created: ${createdTables.get}")
      java.lang.System.out.println(s"$testName.Loaded: ${loadedTables.get}")
      val created: String = (s"unzip -p -a ${createdTables.get}" #| "sum").!!
      val loaded: String = (s"unzip -p -a ${loadedTables.get}" #| "sum").!!

      created shouldEqual loaded

    }
  }

}