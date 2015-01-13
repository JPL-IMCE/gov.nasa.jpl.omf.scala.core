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
import gov.nasa.jpl.omf.scala.core.functionalAPI._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

import scala.language.implicitConversions
import scala.language.postfixOps
import org.scalatest._
import scalaz.Scalaz._

abstract class IMCEMissionDomainTBoxExample[omf <: OMF]()( implicit ops: OMFOps[omf] )
extends WordSpec with Matchers {

  import ops._

  val i_m: omf#IRI = makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission")
  val i_c = makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Component")
  val i_f = makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Function")
  val i_p = makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Performs")

  val c_c = makeEntityConcept(i_c)
  val c_f = makeEntityConcept(i_f)
  val r_cPf = makeEntityRelationship(i_p, c_c, c_f, isAsymmetric, isInverseFunctional)

  val t0 = makeTerminologyGraph(i_m);

  val t1 = t0 withConcept(c_c);

  val t2 = makeTerminologyGraph(i_m) withConcept c_c withEntityRelationship r_cPf;

  val t3 = makeTerminologyGraph(i_m) withConcept makeEntityConcept(i_c) withEntityRelationship r_cPf;

  val t4 = (
    makeTerminologyGraph(i_m)
    withConcept makeEntityConcept(i_c)
    withEntityRelationship r_cPf)

  "basic construction tests" when {
    "empty tbox should be empty" in {
      val (iri, c, r, sc, st, sdr, edr, ax) = ops.fromTerminologyGraph(t0)
      c.isEmpty should be(true)
      r.isEmpty should be(true)
      sc.isEmpty should be(true)
      st.isEmpty should be(true)
      sdr.isEmpty should be(true)
      edr.isEmpty should be(true)
      ax.isEmpty should be(true)
    }
    
    "lookup should find contents" in {
      t2.tbox should be a 'success
      val g2 = t2.graph.tgraph
      g2 containsEntityConcept c_c should be(true)
      g2 containsEntityConcept c_f should be(false)
    }
    
    "equivalent constructions should be isomorphic" in {
       t2.tbox should be a 'success
       t4.tbox should be a 'success
       t2.graph isIsomorphicWith t4.graph
    }
  }

}