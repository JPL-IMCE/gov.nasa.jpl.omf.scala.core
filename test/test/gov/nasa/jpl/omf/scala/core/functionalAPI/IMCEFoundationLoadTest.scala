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

import org.scalatest._

abstract class IMCEFoundationLoadTest[omf <: OMF](
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
          xsd_tbox <- loadTerminology( xsd_iri )
          integer_iri <- withFragment( xsd_iri, "integer" )
          string_iri <- withFragment( xsd_iri, "string" )
        } yield {
          val xsd_integer = lookupDataRange( xsd_tbox._1, integer_iri, recursively=false )
          xsd_integer.isDefined should be( true )

          val xsd_string = lookupDataRange( xsd_tbox._1, string_iri, recursively=false )
          xsd_string.isDefined should be( true )
        }
      result1.isRight should be( true )

      val result2 =
        for {
          base_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" )
          base_tbox <- loadTerminology( base_iri )
          identifiedElement_iri <- withFragment( base_iri, "IdentifiedElement" )
          hasIdentifier_iri <- withFragment( base_iri, "hasIdentifier" )
        } yield {
          val identifiedElement =
            lookupAspect( base_tbox._1, identifiedElement_iri, recursively=false )
          identifiedElement.isDefined should be(true)

          val hasIdentifier =
            lookupEntityScalarDataProperty( base_tbox._1, hasIdentifier_iri, recursively=false )
          hasIdentifier.isDefined should be(true)
        }
      result2.isRight should be (true)

      val result3 =
        for {
          mission_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" )
          mission_tbox <- loadTerminology( mission_iri )
          component_iri <- withFragment( mission_iri, "Component" )
          function_iri <- withFragment( mission_iri, "Function" )
          performs_iri <- withFragment( mission_iri, "Performs" )
        } yield {
          val component =
            lookupConcept( mission_tbox._1, component_iri, recursively=false )
          component.isDefined should be(true)

          val function =
            lookupConcept( mission_tbox._1, function_iri, recursively=false )
          function.isDefined should be(true)

          val component_performs_function =
            lookupReifiedRelationship( mission_tbox._1, performs_iri, recursively=false )
          component_performs_function.isDefined should be(true)
        }
      result3.isRight should be (true)

      val result4 =
        for {
          analysis_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/analysis/analysis" )
          analysis_tbox <- loadTerminology( analysis_iri )
          characterization_iri <- withFragment( analysis_iri, "Characterization" )
          characterizedElement_iri <- withFragment( analysis_iri, "CharacterizedElement" )
        } yield {
          val characterization =
            lookupConcept( analysis_tbox._1, characterization_iri, recursively=false  )
          characterization.isDefined should be(true)

          val characterizedElement =
            lookupAspect( analysis_tbox._1, characterizedElement_iri, recursively=false  )
          characterizedElement.isDefined should be(true)
        }
      result4.isRight should be(true)

      val result5 =
        for {
          behavior_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/behavior/behavior" )
          behavior_tbox <- loadTerminology( behavior_iri )
          stateVariable_iri <- withFragment( behavior_iri, "StateVariable" )
          parameter_iri <- withFragment( behavior_iri, "Parameter" )
        } yield {
          val stateVariable =
            lookupConcept( behavior_tbox._1, stateVariable_iri, recursively=false  )
          stateVariable.isDefined should be(true)

          val parameter =
            lookupConcept( behavior_tbox._1, parameter_iri, recursively=false  )
          parameter.isDefined should be(true)
        }
      result5.isRight should be(true)

      val result6 =
        for {
          project_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/project/project" )
          project_tbox <- loadTerminology( project_iri )
          organization_iri <- withFragment( project_iri, "Organization" )
          workPackage_iri <- withFragment( project_iri, "WorkPackage" )
        } yield {
          val organization =
            lookupConcept( project_tbox._1, organization_iri, recursively=false  )
          organization.isDefined should be(true)

          val workPackage =
            lookupConcept( project_tbox._1, workPackage_iri, recursively=false  )
          workPackage.isDefined should be(true)
        }
      result6.isRight should be (true)
    }
    
  }
}