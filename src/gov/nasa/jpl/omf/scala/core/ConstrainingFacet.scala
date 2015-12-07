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

import scala.{Boolean,Enumeration,Int}
import scala.Predef._

/**
  * Corresponds to an XML Schema 1.1 simple type defined with 'fixed=true'
  *
  * @see http://www.w3.org/TR/xmlschema11-2/#schema
  * <xs:complexType name="facet">
  *   <xs:complexContent>
  *     <xs:extension base="xs:annotated">
  *       <xs:attribute name="value" use="required"/>
  *       <xs:attribute name="fixed" type="xs:boolean" default="false" use="optional"/>
  *     </xs:extension>
  *   </xs:complexContent>
  * </xs:complexType>
  */
trait FixedFacet

/**
  * Corresponds to an XML Schema 1.1 simple type defined with 'fixed=false'
  *
  * @see http://www.w3.org/TR/xmlschema11-2/#schema
  *
  * <xs:complexType name="facet">
  *   <xs:complexContent>
  *     <xs:extension base="xs:annotated">
  *       <xs:attribute name="value" use="required"/>
  *       <xs:attribute name="fixed" type="xs:boolean" default="false" use="optional"/>
  *     </xs:extension>
  *   </xs:complexContent>
  * </xs:complexType>
  */
trait NonFixedFacet

/**
  * OMF ConstrainingFacet corresponds to the normative constrainting facets
  * defined in W3C XML Schema 1.1 Datatypes
  * @see http://www.w3.org/TR/xmlschema11-2/#rf-facets
  */
sealed abstract trait ConstrainingFacet

object ConstrainingFacet {

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-length
    */
  abstract class length(l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class fixedLength(l: Int) extends length(l) with FixedFacet

  case class nonFixedLength(l: Int) extends length(l) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-minLength
    */
  abstract class minLength(l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class fixedMinLength(l: Int) extends minLength(l) with FixedFacet

  case class nonFixedMinLength(l: Int) extends minLength(l) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-maxLength
    */
  abstract class maxLength(l: Int) extends ConstrainingFacet {
    require(l >= 0)
  }

  case class fixedMaxLength(l: Int) extends maxLength(l) with FixedFacet

  case class nonFixedMaxLength(l: Int) extends maxLength(l) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-pattern
    */
  case class pattern(regex: String) extends ConstrainingFacet {
    require(regex.nonEmpty)
  }

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-enumeration
   */
  case class enumeration(values: Set[String]) extends ConstrainingFacet {
    require(values.nonEmpty)
  }

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-whiteSpace
   */
  object WhiteSpaceConstraint extends Enumeration {
    type WhiteSpaceConstraint = Value
    val fixedPreserve, nonFixedPreserve,
    fixedReplace, nonFixedReplace,
    fixedCollapse, nonFixedCollapse = Value

    def isFixed(value: WhiteSpaceConstraint): Boolean =
      value match {
        case _@(WhiteSpaceConstraint.fixedPreserve |
                WhiteSpaceConstraint.fixedReplace |
                WhiteSpaceConstraint.fixedCollapse) => true
        case _                                      => false
      }

    def isNonFixed(value: WhiteSpaceConstraint): Boolean =
      !isFixed(value)
  }

  import WhiteSpaceConstraint._

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-whiteSpace
   */
  case class whiteSpace(constraint: WhiteSpaceConstraint)
    extends ConstrainingFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-maxInclusive
    */
  abstract class maxInclusive(value: String)
    extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class fixedMaxInclusive(value: String)
    extends maxInclusive(value) with FixedFacet

  case class nonFixedMaxInclusive(value: String)
    extends maxInclusive(value) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-maxExclusive
    */
  abstract class maxExclusive(value: String)
    extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class fixedMaxExclusive(value: String)
    extends maxExclusive(value) with FixedFacet

  case class nonFixedMaxExclusive(value: String)
    extends maxExclusive(value) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-minExclusive
    */
  abstract class minExclusive(value: String)
    extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class fixedMinExclusive(value: String)
    extends minExclusive(value) with FixedFacet

  case class nonFixedMinExclusive(value: String)
    extends minExclusive(value) with NonFixedFacet

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-minInclusive
   */
  abstract class minInclusive(value: String)
    extends ConstrainingFacet {
    require(value.nonEmpty)
  }

  case class fixedMinInclusive(value: String)
    extends minInclusive(value) with FixedFacet

  case class nonFixedMinInclusive(value: String)
    extends minInclusive(value) with NonFixedFacet

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-totalDigits
   */
  abstract class totalDigits(digits: Int)
    extends ConstrainingFacet {
    require(digits > 0)
  }

  case class fixedTotalDigits(digits: Int)
    extends totalDigits(digits) with FixedFacet

  case class nonFixedTotalDigits(digits: Int)
    extends totalDigits(digits) with NonFixedFacet

  /**
   * @see http://www.w3.org/TR/xmlschema11-2/#rf-fractionDigits
   */
  abstract class fractionDigits(digits: Int)
    extends ConstrainingFacet {
    require(digits >= 0)
  }

  case class fixedFractionDigits(digits: Int)
    extends fractionDigits(digits) with FixedFacet

  case class nonFixedFractionDigits(digits: Int)
    extends fractionDigits(digits) with NonFixedFacet

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-explicitTimezone
    */
  object ExplicitTimezoneConstraint extends Enumeration {
    type ExplicitTimezoneConstraint = Value
    val fixedRequired, fixedProhibited, fixedOptional, nonFixedOptional = Value

    def isFixed(value: ExplicitTimezoneConstraint): Boolean =
      !isFixed(value)

    def isNonFixed(value: ExplicitTimezoneConstraint): Boolean =
      value == ExplicitTimezoneConstraint.nonFixedOptional
  }

  import ExplicitTimezoneConstraint._

  /**
    * @see http://www.w3.org/TR/xmlschema11-2/#rf-explicitTimezone
    */
  case class explicitTimezone(constraint: ExplicitTimezoneConstraint)
    extends ConstrainingFacet

}