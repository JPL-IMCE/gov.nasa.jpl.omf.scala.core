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
package gov.nasa.jpl.omf.scala.core

import scala.{Boolean,Enumeration}

/**
  * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-fund-facets
  */
sealed abstract trait FundamentalFacet

object FundamentalFacet {

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-ordered
    */
  object OrderedConstraint extends Enumeration {
    type OrderedConstraint = Value
    val _false, partial, total = Value
  }

  import OrderedConstraint._

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-ordered
    */
  case class ordered(constraint: OrderedConstraint)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-bounded
    */
  case class bounded(constraint: Boolean)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-cardinality
    */
  object CardinalityConstraint extends Enumeration {
    type CardinalityConstraint = Value
    val finite, countablyInfinite = Value
  }

  import CardinalityConstraint._

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-cardinality
    */
  case class cardinality(constraint: CardinalityConstraint)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-numeric
    */
  case class numeric(constraint: Boolean)
    extends FundamentalFacet
}
