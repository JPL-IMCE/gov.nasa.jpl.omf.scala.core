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
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest._
import scalaz.Scalaz._

abstract class OMFVocabularyTest[omf <: OMF](
  val saveStore: omf#Store, saveOps: OMFOps[omf],
  val loadStore: omf#Store, loadOps: OMFOps[omf] )
  extends WordSpec with Matchers {

  "vocabulary roundtrip test" when {

    "construct tboxes and save them" in {

      implicit val store = saveStore
      implicit val ops = saveOps
      import ops._

      val xsd = loadTerminologyGraph( makeIRI( "http://www.w3.org/2001/XMLSchema" ) )
      xsd should be a 'success

      val integer = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#integer" ) )
      integer.isDefined should be( true )

      val string = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#string" ) )
      string.isDefined should be( true )

      val base = makeTerminologyGraph(
        makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" ),
        isDefinition )
      base should be a 'success

      val base_extends_xsd = addTerminologyGraphExtension( base.get, xsd.get )
      base_extends_xsd should be a 'success

      val identifiedElement = addEntityAspect( base.get, "IdentifiedElement" )
      identifiedElement should be a 'success

      val hasIdentifier = addDataRelationshipFromEntityToScalar(
        graph = base.get,
        source = identifiedElement.get,
        target = string.get,
        dataRelationshipName = "hasIdentifier" )
      hasIdentifier should be a 'success

      val ibase = asImmutableTerminologyGraph( base.get )
      ibase should be a 'success

      val mission = makeTerminologyGraph(
        makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" ),
        isDefinition )
      mission should be a 'success

      val mission_extends_base = addTerminologyGraphExtension( mission.get, base.get )
      base_extends_xsd should be a 'success

      val component = addEntityConcept( mission.get, "Component", None )
      component should be a 'success

      val component_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
        graph = mission.get,
        sub = component.get._1,
        sup = identifiedElement.get )
      component_extends_identifiedElement.isSuccess should be( true )

      val function = addEntityConcept( mission.get, "Function", None )
      function should be a 'success

      val function_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
        graph = mission.get,
        sub = function.get._1,
        sup = identifiedElement.get )
      function_extends_identifiedElement should be a 'success

      val component_performs_function = addEntityRelationship(
        graph = mission.get,
        source = component.get._1,
        target = function.get._1,
        characteristics = List( isAsymmetric, isIrreflexive, isInverseFunctional ),
        reifiedRelationshipName = "Performs", None,
        unreifiedRelationshipName = "performs",
        unreifiedInverseRelationshipName = Some( "isPerformedBy" ),
        isAbstract = false )
      component_performs_function should be a 'success

      val item = addEntityConcept( mission.get, "Item", None )
      item should be a 'success

      val message = addEntityConcept( mission.get, "Message", None )
      message should be a 'success

      val materialItem = addEntityConcept( mission.get, "MaterialItem", None )
      materialItem should be a 'success

      val message_extends_item = addEntityConceptSubClassAxiom( mission.get, message.get._1, item.get._1 )
      message_extends_item should be a 'success

      val materialItem_extends_item = addEntityConceptSubClassAxiom( mission.get, materialItem.get._1, item.get._1 )
      materialItem_extends_item should be a 'success

      val baseSaved = saveTerminologyGraph( base.get )
      baseSaved should be a 'success

      val missionSaved = saveTerminologyGraph( mission.get )
      missionSaved should be a 'success

    }

    "read tboxes and check them" in {

      implicit val store = loadStore
      implicit val ops = loadOps
      import ops._

      val xsd = loadTerminologyGraph( makeIRI( "http://www.w3.org/2001/XMLSchema" ) )
      xsd should be a 'success

      val integer = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#integer" ) )
      integer.isDefined should be( true )

      val string = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#string" ) )
      string.isDefined should be( true )

      val base = loadTerminologyGraph( makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" ) )
      base should be a 'success

      {
        val ( iri, _, k, i, f, c, r, sc, st, esc, est, ssc, sst, ax ) = ops.fromTerminologyGraph( base.get )
        i.isEmpty should be( false )
        i.toSet.contains( xsd.get ) should be( true )
        f.isEmpty should be( false )
        c.isEmpty should be( true )
        r.isEmpty should be( true )
        sc.isEmpty should be( false )
        st.isEmpty should be( true )
        esc.isEmpty should be( false )
        est.isEmpty should be( true )
        ssc.isEmpty should be( true )
        sst.isEmpty should be( true )
        ax.isEmpty should be( true )
      }

      val identifiedElement = lookupEntityAspect( base.get, makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base#IdentifiedElement" ) )
      identifiedElement.isDefined should be( true )

      val hasIdentifier = lookupEntityDataRelationshipFromEntityToScalar( base.get, makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base#hasIdentifier" ) )
      hasIdentifier.isDefined should be( true )

      val ( _, hasIdentifierSource, hasIdentifierTarget ) = fromDataRelationshipFromEntityToScalar( hasIdentifier.get )
      identifiedElement.get should be( hasIdentifierSource )
      string.get should be( hasIdentifierTarget )

      val mission = loadTerminologyGraph( makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" ) )
      mission should be a 'success

      {
        val ( iri, _, k, i, f, c, r, sc, st, esc, est, ssc, sst, ax ) = ops.fromTerminologyGraph( mission.get )
        i.isEmpty should be( false )
        i.toSet.contains( base.get ) should be( true )
        f.isEmpty should be( true )
        c.isEmpty should be( false )
        r.isEmpty should be( false )
        sc.isEmpty should be( false )
        st.isEmpty should be( true )
        esc.isEmpty should be( true )
        est.isEmpty should be( true )
        ssc.isEmpty should be( true )
        sst.isEmpty should be( true )
        ax.isEmpty should be( false )
      }
      
      val component = lookupEntityConcept( mission.get, makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission#Component" ) )
      component.isDefined should be( true )

      val function = lookupEntityConcept( mission.get, makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission#Function" ) )
      function.isDefined should be( true )

      val component_performs_function = lookupEntityRelationship( mission.get, makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission#Performs" ) )
      component_performs_function.isDefined should be( true )

      val ( _, _, component_performs_functionSource, component_performs_functionTarget, characteristics, component_performs_functionIsAbstract ) = 
        fromEntityRelationship( component_performs_function.get )
      component.get should be( component_performs_functionSource )
      function.get should be( component_performs_functionTarget )
      component_performs_functionIsAbstract should be( false )
    }

  }

}