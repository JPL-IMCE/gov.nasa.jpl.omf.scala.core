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
package gov.nasa.jpl.omf.scala.core

sealed abstract class ConstrainingFacet {

  case class length(val l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class minLength(val l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class maxLength(val l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class pattern(val regex: String) extends ConstrainingFacet {
    require(regex.nonEmpty)
  }

  case class enumeration(val values: Set[String]) extends ConstrainingFacet {
    require(values.nonEmpty)
  }

  object WhiteSpaceConstraint extends Enumeration {
    type WhiteSpaceConstraint = Value
    val preserve, replace, collapse = Value
  }
  import WhiteSpaceConstraint._

  case class whiteSpace(val constraint: WhiteSpaceConstraint) extends ConstrainingFacet

  case class maxExclusive(val value: String) extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class maxInclusive(val value: String) extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class minExclusive(val value: String) extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class minInclusive(val value: String) extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class totalDigits(val digits: Int) extends ConstrainingFacet {
    require(digits > 0)
  }

  case class fractionDigits(val digits: Int) extends ConstrainingFacet {
    require(digits >= 0)
  }

  object ExplicitTimezoneConstraint extends Enumeration {
    type ExplicitTimezoneConstraint = Value
    val required, prohibited, optional = Value
  }
  import ExplicitTimezoneConstraint._

  case class explicitTimezone(val constraint: ExplicitTimezoneConstraint) extends ConstrainingFacet

}