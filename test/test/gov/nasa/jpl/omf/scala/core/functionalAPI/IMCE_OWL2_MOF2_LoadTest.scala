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

import scala.Option
import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest._

import scalaz.NonEmptyList

abstract class IMCE_OWL2_MOF2_LoadTest[omf <: OMF](
  val loadStore: omf#Store,
  val loadOps: OMFOps[omf] )
  extends WordSpec with Matchers {

  implicit val store = loadStore
  implicit val ops = loadOps
  import ops._

  "IMCE OWL2-MOF2 load test" when {

    "load xsd" in {

      val result =
        for {
          xsd_iri <- makeIRI( "http://www.w3.org/2001/XMLSchema" )
          xsd_tbox <- loadTerminologyGraph(xsd_iri)
          integer_iri <- withFragment(xsd_iri, "integer")
          string_iri <- withFragment(xsd_iri, "string")
          xsd_integer = lookupScalarDataType(xsd_tbox._1, integer_iri, recursively = false)
          xsd_string = lookupScalarDataType(xsd_tbox._1, string_iri, recursively = false)
        } yield {
          xsd_integer.isDefined should be(true)
          xsd_string.isDefined should be( true )
        }
      result.isRight should be(true)
    }

    "load annotation" in {

      val result = for {
        annotation_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation" )
        annotation_tbox <- loadTerminologyGraph( annotation_iri )
      } yield ()
      result.isRight should be(true)

    }

    "load owl2-mof2" in {

      val result =
        for {
          owl2_mof2_iri <- makeIRI( "http://imce.jpl.nasa.gov/foundation/owl2-mof2/owl2-mof2" )
          owl2_mof2_tbox <- loadTerminologyGraph(owl2_mof2_iri)
          binaryAssociationEndType_iri <- withFragment(owl2_mof2_iri, "BinaryAssociationEndType")
          binaryAssociation_iri <- withFragment(owl2_mof2_iri, "BinaryAssociation")
          binaryAssociationEndType = lookupEntityConcept(owl2_mof2_tbox._1, binaryAssociationEndType_iri, recursively = false)
          binaryAssociation = lookupEntityReifiedRelationship( owl2_mof2_tbox._1, binaryAssociation_iri, recursively=false  )
        } yield {
          binaryAssociationEndType.isDefined should be(true)
          binaryAssociation.isDefined should be(true)
        }
      result.swap.toOption should be(Option.empty[NonEmptyList[java.lang.Throwable]])
    }
        
  }
}