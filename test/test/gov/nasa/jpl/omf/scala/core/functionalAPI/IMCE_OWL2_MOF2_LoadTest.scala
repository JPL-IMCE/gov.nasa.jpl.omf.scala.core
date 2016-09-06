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

import scala.Option
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