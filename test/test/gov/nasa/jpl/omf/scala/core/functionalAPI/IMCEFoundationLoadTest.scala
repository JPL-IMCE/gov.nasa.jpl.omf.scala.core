/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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

import scala.Option
import scala.language.implicitConversions
import scala.language.postfixOps
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
          xsd_tbox <- loadTerminologyGraph( xsd_iri )
        _ = java.lang.System.out.println("1")
          integer_iri <- withFragment( xsd_iri, "integer" )
          _ = java.lang.System.out.println("2")
          string_iri <- withFragment( xsd_iri, "string" )
          _ = java.lang.System.out.println("3")
        } yield {
          val xsd_integer = lookupScalarDataType( xsd_tbox._1, integer_iri, recursively=false )
          xsd_integer.isDefined should be( true )

          val xsd_string = lookupScalarDataType( xsd_tbox._1, string_iri, recursively=false )
          xsd_string.isDefined should be( true )
        }
      result1.isRight should be( true )

      val result2 =
        for {
          base_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" )
          base_tbox <- loadTerminologyGraph( base_iri )
          identifiedElement_iri <- withFragment( base_iri, "IdentifiedElement" )
          hasIdentifier_iri <- withFragment( base_iri, "hasIdentifier" )
        } yield {
          val identifiedElement =
            lookupEntityAspect( base_tbox._1, identifiedElement_iri, recursively=false )
          identifiedElement.isDefined should be(true)

          val hasIdentifier =
            lookupEntityDataRelationshipFromEntityToScalar( base_tbox._1, hasIdentifier_iri, recursively=false )
          hasIdentifier.isDefined should be(true)
        }
      result2.isRight should be (true)

      val result3 =
        for {
          mission_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" )
          mission_tbox <- loadTerminologyGraph( mission_iri )
          component_iri <- withFragment( mission_iri, "Component" )
          function_iri <- withFragment( mission_iri, "Function" )
          performs_iri <- withFragment( mission_iri, "Performs" )
        } yield {
          val component =
            lookupEntityConcept( mission_tbox._1, component_iri, recursively=false )
          component.isDefined should be(true)

          val function =
            lookupEntityConcept( mission_tbox._1, function_iri, recursively=false )
          function.isDefined should be(true)

          val component_performs_function =
            lookupEntityReifiedRelationship( mission_tbox._1, performs_iri, recursively=false )
          component_performs_function.isDefined should be(true)
        }
      result3.isRight should be (true)

      val result4 =
        for {
          analysis_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/analysis/analysis" )
          analysis_tbox <- loadTerminologyGraph( analysis_iri )
          characterization_iri <- withFragment( analysis_iri, "Characterization" )
          characterizedElement_iri <- withFragment( analysis_iri, "CharacterizedElement" )
        } yield {
          val characterization =
            lookupEntityConcept( analysis_tbox._1, characterization_iri, recursively=false  )
          characterization.isDefined should be(true)

          val characterizedElement =
            lookupEntityAspect( analysis_tbox._1, characterizedElement_iri, recursively=false  )
          characterizedElement.isDefined should be(true)
        }
      result4.isRight should be(true)

      val result5 =
        for {
          behavior_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/behavior/behavior" )
          behavior_tbox <- loadTerminologyGraph( behavior_iri )
          stateVariable_iri <- withFragment( behavior_iri, "StateVariable" )
          parameter_iri <- withFragment( behavior_iri, "Parameter" )
        } yield {
          val stateVariable =
            lookupEntityConcept( behavior_tbox._1, stateVariable_iri, recursively=false  )
          stateVariable.isDefined should be(true)

          val parameter =
            lookupEntityConcept( behavior_tbox._1, parameter_iri, recursively=false  )
          parameter.isDefined should be(true)
        }
      result5.isRight should be(true)

      val result6 =
        for {
          project_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/project/project" )
          project_tbox <- loadTerminologyGraph( project_iri )
          organization_iri <- withFragment( project_iri, "Organization" )
          workPackage_iri <- withFragment( project_iri, "WorkPackage" )
        } yield {
          val organization =
            lookupEntityConcept( project_tbox._1, organization_iri, recursively=false  )
          organization.isDefined should be(true)

          val workPackage =
            lookupEntityConcept( project_tbox._1, workPackage_iri, recursively=false  )
          workPackage.isDefined should be(true)
        }
      result6.isRight should be (true)
    }
    
  }
}