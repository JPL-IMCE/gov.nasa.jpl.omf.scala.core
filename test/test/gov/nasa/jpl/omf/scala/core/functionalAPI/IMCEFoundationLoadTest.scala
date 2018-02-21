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

import scala.{Some, StringContext}
import org.scalatest._

abstract class IMCEFoundationLoadTest[omf <: OMF[omf]](
  val loadStore: omf#Store,
  val loadOps: OMFOps[omf] )
  extends WordSpec with Matchers {

  implicit val store = loadStore
  implicit val ops = loadOps
  import ops._

  "IMCE foundation load test" when {

    "load ontologies" in {

      val result1 =
        for {
          xsd_iri <- makeIRI( "http://www.w3.org/2001/XMLSchema" )
          drc <- loadBuiltinDatatypeMap()
          om <- initializeOntologyMapping(drc)
          xsd_tbox <- loadTerminology(om, xsd_iri )
          (xsd, table1) = xsd_tbox
          integer_iri <- withFragment( xsd_iri, localName("integer") )
          string_iri <- withFragment( xsd_iri, localName("string") )
        } yield {
          val xsd_integer = lookupDataRange( xsd, integer_iri, recursively=false )
          xsd_integer.isDefined should be( true )

          val xsd_string = lookupDataRange( xsd, string_iri, recursively=false )
          xsd_string.isDefined should be( true )
          table1
        }
      result1.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load XMLSchema: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      val result2 =
        for {
          table1 <- result1
          base_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" )
          base_tbox <- loadTerminology( table1, base_iri )
          (base, table2) = base_tbox
          identifiedElement_iri <- withFragment( base_iri, localName("IdentifiedElement") )
          hasIdentifier_iri <- withFragment( base_iri, localName("hasIdentifier") )
        } yield {
          val identifiedElement =
            lookupAspect( base, identifiedElement_iri, recursively=false )
          identifiedElement.isDefined should be(true)

          val hasIdentifier =
            lookupEntityScalarDataProperty( base, hasIdentifier_iri, recursively=false )
          hasIdentifier.isDefined should be(true)
          table1
        }
      result2.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load base: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      val result3 =
        for {
          table2 <- result2
          mission_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" )
          mission_tbox <- loadTerminology( table2, mission_iri )
          (mission, table3) = mission_tbox
          component_iri <- withFragment( mission_iri, localName("Component") )
          function_iri <- withFragment( mission_iri, localName("Function") )
          performs_iri <- withFragment( mission_iri, localName("Performs") )
        } yield {
          val component =
            lookupConcept( mission, component_iri, recursively=false )
          component.isDefined should be(true)

          val function =
            lookupConcept( mission, function_iri, recursively=false )
          function.isDefined should be(true)

          val component_performs_function =
            lookupReifiedRelationship( mission, performs_iri, recursively=false )
          component_performs_function.isDefined should be(true)
          table3
        }
      result3.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load mission: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      val result4 =
        for {
          table3 <- result3
          analysis_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/analysis/analysis" )
          analysis_tbox <- loadTerminology( table3, analysis_iri )
          (analysis, table4) = analysis_tbox
          characterization_iri <- withFragment( analysis_iri, localName("Characterization") )
          characterizedElement_iri <- withFragment( analysis_iri, localName("CharacterizedElement") )
        } yield {
          val characterization =
            lookupConcept( analysis, characterization_iri, recursively=false  )
          characterization.isDefined should be(true)

          val characterizedElement =
            lookupAspect( analysis, characterizedElement_iri, recursively=false  )
          characterizedElement.isDefined should be(true)
          table4
        }
      result4.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load analysis: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      val result5 =
        for {
          table4 <- result4
          behavior_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/behavior/behavior" )
          behavior_tbox <- loadTerminology( table4, behavior_iri )
          (behavior, table5) = behavior_tbox
          stateVariable_iri <- withFragment( behavior_iri, localName("StateVariable") )
          parameter_iri <- withFragment( behavior_iri, localName("Parameter") )
        } yield {
          val stateVariable =
            lookupConcept( behavior, stateVariable_iri, recursively=false  )
          stateVariable.isDefined should be(true)

          val parameter =
            lookupConcept( behavior, parameter_iri, recursively=false  )
          parameter.isDefined should be(true)
          table5
        }
      result5.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load behavior: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      val result6 =
        for {
          table5 <- result5
          project_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/project/project" )
          project_tbox <- loadTerminology( table5, project_iri )
          (project, table6) = project_tbox
          organization_iri <- withFragment( project_iri, localName("Organization") )
          workPackage_iri <- withFragment( project_iri, localName("WorkPackage") )
        } yield {
          val organization =
            lookupConcept( project, organization_iri, recursively=false  )
          organization.isDefined should be(true)

          val workPackage =
            lookupConcept( project, workPackage_iri, recursively=false  )
          workPackage.isDefined should be(true)
          table6
        }
      result6.leftMap { errors =>
        throw new exceptions.TestFailedException(
          message=Some(s"load project: ${errors.size} errors"),
          cause=errors.headOption,
          failedCodeStackDepth = 1
        )
      }

      result6.isRight should be(true)
    }
    
  }
}