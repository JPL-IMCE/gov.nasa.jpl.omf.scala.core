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

abstract class OMFNestedGraphTest[omf <: OMF]
( val saveStore: omf#Store, saveOps: OMFOps[omf],
  val loadStore: omf#Store, loadOps: OMFOps[omf]
) extends WordSpec with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave
  (testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit =

    nonFatalCatch[Unit]
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
        result.isRight should equal(true)
      })


  def preOMFLoad(): Unit

  def postOMFLoad(): Unit

  def withOMFLoad
  (testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
  : Unit =

    nonFatalCatch[Unit]
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
        result.isRight should equal(true)
      })

  "nested graph roundtrip test" when {

    "construct nested tboxes and save them" in withOMFSave { (s, o) =>

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

        base_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/base/base")
        base <- makeTerminologyGraph(base_iri, isDefinition)
        base_extends_xsd <- addTerminologyGraphExtension(base, xsd._1)
        identifiedElement <- addEntityAspect(base, "IdentifiedElement")
        hasIdentifier = addDataRelationshipFromEntityToScalar(
          graph = base,
          source = identifiedElement,
          target = string.get,
          dataPropertyName = "hasIdentifier")

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/mission/mission")
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

        project_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/project/project")
        project <- makeTerminologyGraph(project_iri, isDefinition)
        project_extends_mission <- addTerminologyGraphExtension(project, mission)
        workPackage <- addEntityConcept(project, "WorkPackage", isAbstract = false)

        g_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G")
        g <- makeTerminologyGraph(g_iri, isDefinition)
        g_extends_project <- addTerminologyGraphExtension(g, project)

        g_A <- addEntityConcept(g, "A", isAbstract=false)
        g_A_isa_component <- addEntityConceptSubClassAxiom(g, g_A, component)

        g_B <- addEntityConcept(g, "B", isAbstract=false)
        g_B_isa_component <- addEntityConceptSubClassAxiom(g, g_B, component)

        g_C <- addEntityConcept(g, "C", isAbstract=false)
        g_C_isa_function <- addEntityConceptSubClassAxiom(g, g_C, component)

        p1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/P1")
        p1 <- makeTerminologyGraph(p1_iri, isDefinition)
        g_authorizes_p1 <- addEntityConcept(g, "P1", isAbstract=false)
        g_authorizes_p1_WP <- addEntityConceptSubClassAxiom(g, g_authorizes_p1, workPackage)
        g_nests_p1 <- addNestedTerminologyGraph(nestingParent=g, nestingContext=g_authorizes_p1, nestedChild=p1)

        p1_asserts_A_performs_C <- addEntityReifiedRelationship(
          graph = p1,
          source = g_A,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        p1_asserts_A_performsFunction_C <-
        addEntityReifiedRelationshipSubClassAxiom(graph=p1, sub=p1_asserts_A_performs_C, sup=component_performs_function)

        p2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/P2")
        p2 <- makeTerminologyGraph(p2_iri, isDefinition)
        g_authorizes_p2 <- addEntityConcept(g, "P2", isAbstract=false)
        g_authorizes_p2_WP <- addEntityConceptSubClassAxiom(g, g_authorizes_p2, workPackage)
        g_nests_p2 <- addNestedTerminologyGraph(nestingParent=g, nestingContext=g_authorizes_p2, nestedChild=p2)

        p2_asserts_B_performs_C <- addEntityReifiedRelationship(
          graph = p2,
          source = g_B,
          target = g_C,
          characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
          reifiedRelationshipName = "Performs",
          unreifiedRelationshipName = "performs",
          unreifiedInverseRelationshipName = "isPerformedBy".some,
          isAbstract = false)
        p2_asserts_B_performsFunction_C <-
        addEntityReifiedRelationshipSubClassAxiom(graph=p2, sub=p2_asserts_B_performs_C, sup=component_performs_function)

        ibase <- asImmutableTerminologyGraph(base)
        _ <- saveTerminologyGraph(ibase._1)

        imission <- asImmutableTerminologyGraph(mission)
        _ <- saveTerminologyGraph(imission._1)

        iproject <- asImmutableTerminologyGraph(project)
        _ <- saveTerminologyGraph(iproject._1)

        ig <- asImmutableTerminologyGraph(g)
        _ <- saveTerminologyGraph(ig._1)

        ip1 <- asImmutableTerminologyGraph(p1)
        _ <- saveTerminologyGraph(ip1._1)

        ip2 <- asImmutableTerminologyGraph(p2)
        _ <- saveTerminologyGraph(ip2._1)

      } yield {
        lookupNestingAxiomForNestedChildIfAny(nestedG = g).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ig._1).isEmpty should be(true)

        lookupNestingAxiomForNestedChildIfAny(nestedG = p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip1._1).foreach { ax =>
          getNestingParentGraphOfAxiom(ax) should be(ig._1)
        }

        lookupNestingAxiomForNestedChildIfAny(nestedG = p2).contains(g_nests_p2) should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = ip2._1).foreach { ax =>
          getNestingParentGraphOfAxiom(ax) should be(ig._1)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1).contains(g_nests_p1) should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2).contains(g_nests_p2) should be(true)

        lookupNestingAxiomsForNestingParent(nestingG = ip1._1).isEmpty should be(true)
        lookupNestingAxiomsForNestingParent(nestingG = p1).isEmpty should be(true)

        lookupNestingAxiomsForNestingParent(nestingG = ip2._1).isEmpty should be(true)
        lookupNestingAxiomsForNestingParent(nestingG = p2).isEmpty should be(true)

        lookupNestingAxiomsForNestingParent(nestingG = g).contains(g_nests_p1) should be(true)
        lookupNestingAxiomsForNestingParent(nestingG = g).contains(g_nests_p2) should be(true)

        // confirm there is a nested graph axiom for the immutable child P1
        lookupNestingAxiomsForNestingParent(nestingG = ig._1)
          .exists { ax => getNestedChildGraphOfAxiom(ax) == ip1._1 } should be(true)

        // confirm there is a nested graph axiom for the immutable child P2
        lookupNestingAxiomsForNestingParent(nestingG = ig._1)
          .exists { ax => getNestedChildGraphOfAxiom(ax) == ip2._1 } should be(true)
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

        mission_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/mission/mission")
        mission <- loadTerminologyGraph(mission_iri)

        component_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/mission/mission#Component")
        component = lookupEntityConcept(mission._1, component_iri, recursively=false)

        function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/mission/mission#Function")
        function = lookupEntityConcept(mission._1, function_iri, recursively=false)

        component_performs_function_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/mission/mission#Performs")

        project_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/project/project")
        project <- loadTerminologyGraph(project_iri)

        workPackage_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/foundation/project/project#WorkPackage")

        g_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G")
        g <- loadTerminologyGraph(g_iri)

        a_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G#A")
        a = lookupEntityConcept(g._1, a_iri, recursively=false)

        b_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G#B")
        b = lookupEntityConcept(g._1, b_iri, recursively=false)

        c_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G#C")
        c = lookupEntityConcept(g._1, c_iri, recursively=false)

        p1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/P1")
        p1 <- loadTerminologyGraph(p1_iri)

        g_authorizes_p1_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G#P1")
        g_authorizes_p1 = lookupEntityConcept(g._1, g_authorizes_p1_iri, recursively=false)

        p2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/P2")
        p2 <- loadTerminologyGraph(p2_iri)

        g_authorizes_p2_iri <- makeIRI("http://imce.jpl.nasa.gov/test/nestedGraph/example/G#P2")
        g_authorizes_p2 = lookupEntityConcept(g._1, g_authorizes_p2_iri, recursively=false)

      } yield {
        a.isDefined should be(true)
        b.isDefined should be(true)
        c.isDefined should be(true)
        g_authorizes_p1.isDefined should be(true)
        g_authorizes_p2.isDefined should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = g._1).isEmpty should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1._1).isDefined should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p1._1).foreach { ax =>
          getNestingParentGraphOfAxiom(ax) should be(g._1)
        }
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2._1).isDefined should be(true)
        lookupNestingAxiomForNestedChildIfAny(nestedG = p2._1).foreach { ax =>
          getNestingParentGraphOfAxiom(ax) should be(g._1)
        }

        lookupNestingAxiomsForNestingContext(nestingC = component.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = function.get).isEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p1.get).nonEmpty should be(true)
        lookupNestingAxiomsForNestingContext(nestingC = g_authorizes_p2.get).nonEmpty should be(true)

        lookupNestingAxiomsForNestingParent(nestingG = p1._1).isEmpty should be(true)
        lookupNestingAxiomsForNestingParent(nestingG = p1._1).isEmpty should be(true)
        lookupNestingAxiomsForNestingParent(nestingG = g._1).size should be(4) // mutable & immutable

        // confirm there is a nested graph axiom for the immutable child P1
        lookupNestingAxiomsForNestingParent(nestingG = g._1)
          .exists { ax => getNestedChildGraphOfAxiom(ax) == p1._1 } should be(true)

        // confirm there is a nested graph axiom for the immutable child P2
        lookupNestingAxiomsForNestingParent(nestingG = g._1)
          .exists { ax => getNestedChildGraphOfAxiom(ax) == p2._1 } should be(true)

        lookupNestingAxiomsForNestingParent(nestingG = g._1).foreach { ax =>

          val parentG = getNestingParentGraphOfAxiom(ax)
          val parentC = getNestingContextConceptOfAxiom(ax)
          val childG = getNestedChildGraphOfAxiom(ax)

          java.lang.System.out.println(
            "Nesting:\n"+
              s" parent: ${parentG.getClass.getSimpleName}: ${getTerminologyGraphIRI(parentG)}\n"+
              s" context: ${fromEntityConcept(parentC).iri}\n"+
              s" child: ${childG.getClass.getSimpleName}: ${getTerminologyGraphIRI(childG)}\n"
          )

        }

      }

    }
  }

}