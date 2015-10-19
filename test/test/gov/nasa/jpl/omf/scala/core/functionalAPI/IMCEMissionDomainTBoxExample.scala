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
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest.{Matchers,WordSpec}

abstract class IMCEMissionDomainTBoxExample[omf <: OMF]()(
  implicit val ops: OMFOps[omf],
  implicit val store: omf#Store )
  extends WordSpec with Matchers {

  import ops._

  "basic construction tests" when {
    "empty tbox should be empty" in {

      val result =
        for {
          i_m0 <- makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission0" )
          g <- makeTerminologyGraph(i_m0, isDefinition)
          s = ops.fromTerminologyGraph(g)
        } yield {
          s.imports.isEmpty should be(true)
          s.aspects.isEmpty should be(true)
          s.concepts.isEmpty should be(true)
          s.reifiedRelationships.isEmpty should be(true)
          s.unreifiedRelationships.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.scalarDataTypes.isEmpty should be(true)
          s.entity2scalarDataRelationships.isEmpty should be(true)
          s.entity2structureDataRelationships.isEmpty should be(true)
          s.structure2scalarDataRelationships.isEmpty should be(true)
          s.structure2structureDataRelationships.isEmpty should be(true)
          s.axioms.isEmpty should be(true)
        }
      result.isRight should be(true)
    }

    "simple construction & lookup" in {

      val result =
        for {
          i_m1 <- makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission1" )
          g <- makeTerminologyGraph(i_m1, isDefinition)
          component <- addEntityConcept(g, "Component", isAbstract = false)
          function <- addEntityConcept(g, "Function", isAbstract = false)
          s = fromTerminologyGraph(g)
        } yield {
          s.iri should be(i_m1)
          s.imports.isEmpty should be(true)
          s.aspects.isEmpty should be(true)

          s.concepts.nonEmpty should be(true)
          s.concepts.size should be(2)
          s.concepts.toSet.contains(component) should be(true)
          s.concepts.toSet.contains(function) should be(true)

          s.reifiedRelationships.isEmpty should be(true)
          s.scalarDataTypes.isEmpty should be(true)
          s.structuredDataTypes.isEmpty should be(true)
          s.entity2scalarDataRelationships.isEmpty should be(true)
          s.entity2structureDataRelationships.isEmpty should be(true)
          s.structure2scalarDataRelationships.isEmpty should be(true)
          s.structure2structureDataRelationships.isEmpty should be(true)
          s.axioms.isEmpty should be(true)
        }
      result.isRight should be(true)
    }
  }

}