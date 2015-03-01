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

abstract class IMCEMissionDomainTBoxExample[omf <: OMF]()( implicit ops: OMFOps[omf], store: omf#Store )
  extends WordSpec with Matchers {

  import ops._

  val i_m0: omf#IRI = makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission0" )
  val i_m1: omf#IRI = makeIRI( "http://imce.jpl.nasa.gov/foundation/mission/mission1" )

  "basic construction tests" when {
    "empty tbox should be empty" in {

      val t0 = makeTerminologyGraph( i_m0, isDefinition )
      t0.isSuccess should be( true )

      val ( iri, k, i, f, c, r, sc, st, esc, est, ssc, sst, ax ) = ops.fromTerminologyGraph( t0.get )
      i.isEmpty should be( true )
      f.isEmpty should be( true )
      c.isEmpty should be( true )
      r.isEmpty should be( true )
      sc.isEmpty should be( true )
      st.isEmpty should be( true )
      esc.isEmpty should be( true )
      est.isEmpty should be( true )
      ssc.isEmpty should be( true )
      sst.isEmpty should be( true )
      ax.isEmpty should be( true )
    }

    "simple construction & lookup" in {

      val t1 = makeTerminologyGraph( i_m1, isDefinition )
      t1.isSuccess should be( true )

      val g = t1.get

      val component = addEntityConcept( g, "Component", None )
      component.isSuccess should be( true )

      val function = addEntityConcept( g, "Function", None )
      function.isSuccess should be( true )

      val ( iri, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( g )
      iri should be( i_m1 )
      _i.isEmpty should be( true )
      _f.isEmpty should be( true )

      _c.nonEmpty should be( true )
      _c.iterator.size should be( 2 )
      _c.iterator.contains( component.get ) should be( true )
      _c.iterator.contains( function.get ) should be( true )

      _r.isEmpty should be( true )
      _sc.isEmpty should be( true )
      _st.isEmpty should be( true )
      _esc.isEmpty should be( true )
      _est.isEmpty should be( true )
      _ssc.isEmpty should be( true )
      _sst.isEmpty should be( true )
      _ax.isEmpty should be( true )
    }
  }

}