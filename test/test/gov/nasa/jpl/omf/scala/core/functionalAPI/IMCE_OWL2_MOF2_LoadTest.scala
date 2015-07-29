/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
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
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest._
import scalaz.Scalaz._
import scala.util.Try

abstract class IMCE_OWL2_MOF2_LoadTest[omf <: OMF](
  val loadStore: omf#Store,
  val loadOps: OMFOps[omf] )
  extends WordSpec with Matchers {

  implicit val store = loadStore
  implicit val ops = loadOps
  import ops._

  "IMCE OWL2-MOF2 load test" when {

    var xsd_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    var xsd_integer: Option[omf#ModelScalarDataType] = null
    var xsd_string: Option[omf#ModelScalarDataType] = null

    var annotation_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    
    var owl2_mof2_tbox: Try[omf#ImmutableModelTerminologyGraph] = null
    
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

    "load annotation" in {
      val annotation_iri = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation" )
      annotation_tbox = loadTerminologyGraph( annotation_iri )
      annotation_tbox should be a 'success
    }

    "load owl2-mof2" in {
      val owl2_mof2_iri = makeIRI( "http://imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2" )
      owl2_mof2_tbox = loadTerminologyGraph( owl2_mof2_iri )
      owl2_mof2_tbox should be a 'success

      for {
        BinaryAssociationEndType_iri <- withFragment( owl2_mof2_iri, "BinaryAssociationEndType" )
        BinaryAssociation_iri <- withFragment( owl2_mof2_iri, "BinaryAssociation" )
      } {
        val BinaryAssociationEndType = lookupEntityConcept( owl2_mof2_tbox.get, BinaryAssociationEndType_iri )
        BinaryAssociationEndType.isDefined should be(true)

        val BinaryAssociation = lookupEntityRelationship( owl2_mof2_tbox.get, BinaryAssociation_iri )
        BinaryAssociation.isDefined should be(true)
      }
    }
        
  }
}