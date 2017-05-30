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

import gov.nasa.jpl.imce.oml.tables.OMLSpecificationTables
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import exceptions._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.OMLString.LocalName
import gov.nasa.jpl.omf.scala.core.tables.OMFTabularExport

import scala.collection.mutable.ArrayBuffer
import scala.{Some, StringContext, Unit}
import scala.Predef.String
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import scala.collection.immutable.{List, Set}

abstract class OMFNestedGraphTest[omf <: OMF]
( testName: String,
  val saveStore: omf#Store, saveOps: OMFOps[omf],
  val loadStore: omf#Store, loadOps: OMFOps[omf]
) extends WordSpec with Matchers {

  val createdTables: ArrayBuffer[File] = ArrayBuffer.empty
  val loadedTables: ArrayBuffer[File] = ArrayBuffer.empty

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
        xsd_table <- loadTerminology(Mutable2ImmutableModuleTable.empty[omf], xsd_iri)
        (xsd, table1) = xsd_table
        int_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#integer")
        integer = lookupDataRange(xsd, int_iri, recursively = false)
        string_iri <- makeIRI("http://www.w3.org/2001/XMLSchema#string")
        string = lookupDataRange(xsd, string_iri, recursively = false)

        base_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyExtension(base, xsd)
        identifiedElement <- addAspect(base, LocalName("IdentifiedElement"))
        hasIdentifier = addEntityScalarDataProperty(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = LocalName("hasIdentifier"),
          isIdentityCriteria = false)

        mission_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission")
        mission <- makeTerminologyGraph(mission_iri, isDefinition)
        mission_extends_base <- addTerminologyExtension(mission, base)
        component <- addConcept(mission, LocalName("Component"))
        component_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = component,
          sup = identifiedElement)
        function <- addConcept(mission, LocalName("Function"))
        function_extends_identifiedElement <- addAspectSpecializationAxiom(
          graph = mission,
          sub = function,
          sup = identifiedElement)
        component_performs_function <- addReifiedRelationship(
          graph = mission,
          source = component,
          target = function,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = LocalName("Performs"),
          unreifiedRelationshipName = LocalName("performs"),
          unreifiedInverseRelationshipName = LocalName("isPerformedBy").some)

        project_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project")
        project <- makeTerminologyGraph(project_iri, isDefinition)
        project_extends_mission <- addTerminologyExtension(project, mission)
        workPackage <- addConcept(project, LocalName("WorkPackage"))

        g_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G")
        g <- makeTerminologyGraph(g_iri, isDefinition)
        g_extends_project <- addTerminologyExtension(g, project)

        g_A <- addConcept(g, LocalName("A"))
        g_A_isa_component <- addConceptSpecializationAxiom(g, g_A, component)

        g_B <- addConcept(g, LocalName("B"))
        g_B_isa_component <- addConceptSpecializationAxiom(g, g_B, component)

        g_C <- addConcept(g, LocalName("C"))
        g_C_isa_function <- addConceptSpecializationAxiom(g, g_C, component)

        p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P1")
        p1 <- makeTerminologyGraph(p1_iri, isDefinition)
        _ <- addTerminologyExtension(p1, g)
        g_authorizes_p1 <- addConcept(g, LocalName("P1"))
        g_authorizes_p1_WP <- addConceptSpecializationAxiom(g, g_authorizes_p1, workPackage)

        p1_asserts_A_performs_C <- addReifiedRelationship(
          graph = p1,
          source = g_A,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = LocalName("Performs"),
          unreifiedRelationshipName = LocalName("performs"),
          unreifiedInverseRelationshipName = LocalName("isPerformedBy").some)
        p1_asserts_A_performsFunction_C <-
        addReifiedRelationshipSpecializationAxiom(graph=p1, sub=p1_asserts_A_performs_C, sup=component_performs_function)

        g_nests_p1 <- addNestedTerminology(nestingGraph=g, nestingContext=g_authorizes_p1, nestedGraph=p1)

        p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P2")
        p2 <- makeTerminologyGraph(p2_iri, isDefinition)
        _ <- addTerminologyExtension(p2, g)
        g_authorizes_p2 <- addConcept(g, LocalName("P2"))
        g_authorizes_p2_WP <- addConceptSpecializationAxiom(g, g_authorizes_p2, workPackage)

        p2_asserts_B_performs_C <- addReifiedRelationship(
          graph = p2,
          source = g_B,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = LocalName("Performs"),
          unreifiedRelationshipName = LocalName("performs"),
          unreifiedInverseRelationshipName = LocalName("isPerformedBy").some)
        p2_asserts_B_performsFunction_C <-
        addReifiedRelationshipSpecializationAxiom(graph=p2, sub=p2_asserts_B_performs_C, sup=component_performs_function)

        g_nests_p2 <- addNestedTerminology(nestingGraph=g, nestingContext=g_authorizes_p2, nestedGraph=p2)

        m2i_base <- asImmutableTerminologyGraph(base, table1)
        (ibase, table2) = m2i_base
        _ <- saveTerminology(ibase)

        m2i_mission <- asImmutableTerminologyGraph(mission, table2)
        (imission, table3) = m2i_mission

        _ <- saveTerminology(imission)

        m2i_project <- asImmutableTerminologyGraph(project, table3)
        (iproject, table4) = m2i_project

        _ <- saveTerminology(iproject)

        m2i_g <- asImmutableTerminologyGraph(g, table4)
        (ig, table5) = m2i_g
        _ <- saveTerminology(ig)

        m2i_p1 <- asImmutableTerminologyGraph(p1, table5)
        (ip1, table6) = m2i_p1
        _ <- saveTerminology(ip1)

        m2i_p2 <- asImmutableTerminologyGraph(p2, table6)
        (ip2, table7) = m2i_p2
        _ <- saveTerminology(ip2)

        tables <- OMFTabularExport.toTables(table7.values)

        _ <- tables.foldLeft(().right[Throwables]) { case (acc, (im, t)) =>

          for {
          _ <- acc
          tablesIRI <- makeIRI(getModuleIRI(im).toString + ".oml.json.zip")
          tablesFile <- resolveIRIAsLocalFile(tablesIRI)

          _ = java.nio.file.Files.createDirectories(tablesFile.getParentFile.toPath)

          _ <-  OMLSpecificationTables.saveOMLSpecificationTables(t, tablesFile)
              .toDisjunction
              .leftMap(Set[java.lang.Throwable](_))

          _ = java.lang.System.out.println(s"as created: $tablesFile")

          _ = createdTables += tablesFile
          } yield ()
        }

      } yield {
        lookupNestingAxiomForNestedChildIfAny(nestedG = g).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ig).isEmpty should be(true)

        lookupNestingAxiomForNestedChildIfAny(nestedG = p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p1)
        }

        lookupNestingAxiomForNestedChildIfAny(nestedG = p2).contains(g_nests_p2) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip2).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p2)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2).contains(g_nests_p2) should be(true)

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

        base_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/base/base")
        base_table <- loadTerminology(table1, base_iri)
        (base, table2) = base_table

        mission_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission")
        mission_table <- loadTerminology(table2, mission_iri)
        (mission, table3) = mission_table

        component_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Component")
        component = lookupConcept(mission, component_iri, recursively=false)

        function_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Function")
        function = lookupConcept(mission, function_iri, recursively=false)

        component_performs_function_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/mission/mission#Performs")

        project_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project")
        project_table <- loadTerminology(table3, project_iri)
        (project, table4) = project_table

        workPackage_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/foundation/project/project#WorkPackage")

        g_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G")
        g_table <- loadTerminology(table4, g_iri)
        (g, table5) = g_table

        a_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#A")
        a = lookupConcept(g, a_iri, recursively=false)

        b_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#B")
        b = lookupConcept(g, b_iri, recursively=false)

        c_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#C")
        c = lookupConcept(g, c_iri, recursively=false)

        p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P1")
        p1_table <- loadTerminology(table5, p1_iri)
        (p1, table6) = p1_table

        g_authorizes_p1_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#P1")
        g_authorizes_p1 = lookupConcept(g, g_authorizes_p1_iri, recursively=false)

        p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/P2")
        p2_table <- loadTerminology(table6, p2_iri)
        (p2, table7) = p2_table

        g_authorizes_p2_iri <- makeIRI(s"http://imce.jpl.nasa.gov/test/$testName/example/G#P2")
        g_authorizes_p2 = lookupConcept(g, g_authorizes_p2_iri, recursively=false)

        tables <- OMFTabularExport.toTables(table7.values)

        _ <- tables.foldLeft(().right[Set[java.lang.Throwable]]) { case (acc, (im, t)) =>

          for {
            _ <- acc
            tablesIRI <- makeIRI(getModuleIRI(im).toString + ".oml.json.zip")
            tablesFile <- resolveIRIAsLocalFile(tablesIRI)

            _ = java.nio.file.Files.createDirectories(tablesFile.getParentFile.toPath)

            _ <-  OMLSpecificationTables.saveOMLSpecificationTables(t, tablesFile)
              .toDisjunction
              .leftMap(Set[java.lang.Throwable](_))

            _ = java.lang.System.out.println(s"as loaded: $tablesFile")

            _ = loadedTables += tablesFile
          } yield ()
        }

      } yield {
        a.isDefined should be(true)
        b.isDefined should be(true)
        c.isDefined should be(true)
        g_authorizes_p1.isDefined should be(true)
        g_authorizes_p2.isDefined should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1).nonEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = g).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p1.get)
        }
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2).nonEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2).foreach { ax =>
          fromTerminologyNestingAxiom(ax).nestingContext should be(g_authorizes_p2.get)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1.get).nonEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2.get).nonEmpty should be(true)

        ()
      }

    }

    "compare created vs loaded" in {
      import scala.sys.process._

      createdTables.nonEmpty should be(true)
      loadedTables.nonEmpty should be(true)
      java.lang.System.out.println(s"$testName.Created: ${createdTables.size}")
      java.lang.System.out.println(s"$testName.Loaded: ${loadedTables.size}")
      createdTables.size shouldEqual loadedTables.size
      (createdTables zip loadedTables).foreach { case (ci, li) =>
        java.lang.System.out.println(s"$testName => Created: $ci")
        java.lang.System.out.println(s"$testName => Loaded: $li")
        val created: String = (s"unzip -p -a $ci" #| "sum").!!
        val loaded: String = (s"unzip -p -a $li" #| "sum").!!
        created shouldEqual loaded
      }
    }
  }

}