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

abstract class OMFVocabularyTest[omf <: OMF]()( implicit ops: OMFOps[omf], store: omf#Store )
  extends WordSpec with Matchers {

  import ops._
  
  "vocabulary coverage test" when {
    
    "empty tbox should be empty" in {
      
      val xsd = loadTerminologyGraph( makeIRI( "http://www.w3.org/2001/XMLSchema") ) 
      xsd should be a 'success
      
      val integer = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#integer" ) )
      integer.isDefined should be(true)
            
      val string = lookupScalarDataType( xsd.get, makeIRI( "http://www.w3.org/2001/XMLSchema#string" ) )
      string.isDefined should be(true)
      
      val base = makeTerminologyGraph( 
          makeIRI( "http://imce.jpl.nasa.gov/foundation/base/base" ), 
          extendedTGraphs=List( xsd.get ) )         
      base should be a 'success
       
      val identifiedElement = addEntityAspect( base.get, "IdentifiedElement" )
      identifiedElement should be a 'success
      
      val hasIdentifier = addDataRelationshipFromEntityToScalar(
          graph=base.get, 
          source=identifiedElement.get, 
          target=string.get, 
          dataRelationshipName="hasIdentifier")
      hasIdentifier should be a 'success       
      
      val mission = makeTerminologyGraph( 
          makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission" ), 
          extendedTGraphs=List( base.get ) )
      mission should be a 'success
       
      val component = addEntityConcept( mission.get, "Component" )
      component should be a 'success

      val component_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
          graph=mission.get,
          sub=component.get,
          sup=identifiedElement.get)
      component_extends_identifiedElement.isSuccess should be( true )
      
      val function = addEntityConcept( mission.get, "Function" )
      function should be a 'success
             
      val function_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
          graph=mission.get,
          sub=function.get,
          sup=identifiedElement.get)
      function_extends_identifiedElement should be a 'success
      
      val component_performs_function = addEntityRelationship(
          graph=mission.get, 
          source=component.get,
          target=function.get, 
          characteristics=List( isAsymmetric, isIrreflexive, isInverseFunctional ),
          reifiedRelationshipName="Performs", 
          unreifiedRelationshipName="performs", 
          unreifiedInverseRelationshipName=Some( "isPerformedBy" ))
      component_performs_function should be a 'success
            
      val item = addEntityConcept( mission.get, "Item" )
      item should be a 'success
  
      val message = addEntityConcept( mission.get, "Message" )
      message should be a 'success

      val materialItem = addEntityConcept( mission.get, "MaterialItem" )
      materialItem should be a 'success

      val message_extends_item = addEntityConceptSubClassAxiom( mission.get, message.get, item.get )
      message_extends_item should be a 'success

      val materialItem_extends_item = addEntityConceptSubClassAxiom( mission.get, materialItem.get, item.get )
      materialItem_extends_item should be a 'success

    }
  }

}