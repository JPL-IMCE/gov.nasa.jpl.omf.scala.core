/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest._
import scalaz.Scalaz._
import scala.util.Try

abstract class IMCEFoundationLoadTest[omf <: OMF](
  val loadStore: omf#Store,
  val loadOps: OMFOps[omf] )
  extends WordSpec with Matchers {

  implicit val store = loadStore
  implicit val ops = loadOps
  import ops._

  "IMCE foundation load test" when {

    var xsd_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    var xsd_integer: Option[omf#ModelScalarDataType] = null
    var xsd_string: Option[omf#ModelScalarDataType] = null

    var base_tbox: Try[omf#ImmutableModelTerminologyGraph] = null

    var mission_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    var analysis_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    var behavior_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    var project_tbox: Try[omf#ImmutableModelTerminologyGraph] = null

    "load xsd" in {
      val xsd_iri = makeIRI( "http://www.w3.org/2001/XMLSchema" )

      xsd_tbox = loadTerminologyGraph( xsd_iri )
      xsd_tbox should be a 'success

      for {
        integer_iri <- withFragment( xsd_iri, "integer" )
        string_iri <- withFragment( xsd_iri, "string" )
      } {
        xsd_integer = lookupScalarDataType( xsd_tbox.get, integer_iri )
        xsd_integer.isDefined should be( true )

        xsd_string = lookupScalarDataType( xsd_tbox.get, string_iri )
        xsd_string.isDefined should be( true )
      }
    }

    "load base" in {
      val base_iri = makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" )
      base_tbox = loadTerminologyGraph( base_iri )
      base_tbox should be a 'success

      for {
        identifiedElement_iri <- withFragment( base_iri, "IdentifiedElement" )
        hasIdentifier_iri <- withFragment( base_iri, "hasIdentifier" )
      } {
        val identifiedElement = lookupEntityAspect( base_tbox.get, identifiedElement_iri )
        identifiedElement.isDefined should be(true)

        val hasIdentifier = lookupEntityDataRelationshipFromEntityToScalar( base_tbox.get, hasIdentifier_iri )
        hasIdentifier.isDefined should be(true)
      }
    }

    "load mission" in {
      val mission_iri = makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" )
      mission_tbox = loadTerminologyGraph( mission_iri )
      mission_tbox should be a 'success

      for {
        component_iri <- withFragment( mission_iri, "Component" )
        function_iri <- withFragment( mission_iri, "Function" )
        performs_iri <- withFragment( mission_iri, "Performs" )
      } {
        val component = lookupEntityConcept( mission_tbox.get, component_iri )
        component.isDefined should be(true)

        val function = lookupEntityConcept( mission_tbox.get, function_iri )
        function.isDefined should be(true)

        val component_performs_function = lookupEntityRelationship( mission_tbox.get, performs_iri )
        component_performs_function.isDefined should be(true)
      }
    }
        
    "load analysis" in {
      val analysis_iri= makeIRI( "http://imce.jpl.nasa.gov/foundation/analysis/analysis" )
      analysis_tbox = loadTerminologyGraph( analysis_iri )
      analysis_tbox should be a 'success

      for {
        characterization_iri <- withFragment( analysis_iri, "Characterization" )
        characterizedElement_iri <- withFragment( analysis_iri, "CharacterizedElement" )
      } {
        val characterization = lookupEntityConcept( analysis_tbox.get, characterization_iri )
        characterization.isDefined should be(true)

        val characterizedElement = lookupEntityAspect( analysis_tbox.get, characterizedElement_iri )
        characterizedElement.isDefined should be(true)
      }
    }
    
    "load behavior" in {
      val behavior_iri= makeIRI( "http://imce.jpl.nasa.gov/foundation/behavior/behavior" )
      behavior_tbox = loadTerminologyGraph( behavior_iri )
      behavior_tbox should be a 'success

      for {
        stateVariable_iri <- withFragment( behavior_iri, "StateVariable" )
        parameter_iri <- withFragment( behavior_iri, "Parameter" )
      } {
        val stateVariable = lookupEntityConcept( behavior_tbox.get, stateVariable_iri )
        stateVariable.isDefined should be(true)

        val parameter = lookupEntityConcept( behavior_tbox.get, parameter_iri )
        parameter.isDefined should be(true)
      }
    }
    
    "load project" in {
      val project_iri= makeIRI( "http://imce.jpl.nasa.gov/foundation/project/project" )
      project_tbox = loadTerminologyGraph( project_iri )
      project_tbox should be a 'success

      for {
        organization_iri <- withFragment( project_iri, "Organization" )
        workPackage_iri <- withFragment( project_iri, "WorkPackage" )
      } {
        val organization = lookupEntityConcept( project_tbox.get, organization_iri )
        organization.isDefined should be(true)

        val workPackage = lookupEntityConcept( project_tbox.get, workPackage_iri )
        workPackage.isDefined should be(true)
      }
    }
    
  }
}