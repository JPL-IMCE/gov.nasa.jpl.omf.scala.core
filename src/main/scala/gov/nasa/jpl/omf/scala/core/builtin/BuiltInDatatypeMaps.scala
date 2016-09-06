/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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
package gov.nasa.jpl.omf.scala.core.builtin

import gov.nasa.jpl.omf.scala.core.ConstrainingFacet._
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet.ExplicitTimezoneConstraint._
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet.WhiteSpaceConstraint._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet.CardinalityConstraint._
import gov.nasa.jpl.omf.scala.core.FundamentalFacet.OrderedConstraint._
import gov.nasa.jpl.omf.scala.core.{OMFOps, OMF}

import scala.collection.immutable._
import scalaz.\/

object BuiltInDatatypeMaps {

  def createBuiltInDatatypeMaps[omf <: OMF]
  (makeW3CTerminologyGraphDefinition: omf#IRI => Set[java.lang.Throwable] \/ omf#MutableModelTerminologyGraph)
  (implicit
   ops: OMFOps[omf],
   store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableModelTerminologyGraph, Map[omf#MutableModelTerminologyGraph, omf#ImmutableModelTerminologyGraph])
  = {
    import ops._

    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd_mgraph <- makeW3CTerminologyGraphDefinition(xsd_iri)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyAtomicType
      anyAtomicType <- addScalarDataType(xsd_mgraph, "anyAtomicType")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyURI
      anyURI <- addScalarDataType(xsd_mgraph, "anyURI")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, anyURI, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#base64Binary
      base64Binary <- addScalarDataType(xsd_mgraph, "base64Binary")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, base64Binary, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#hexBinary
      hexBinary <- addScalarDataType(xsd_mgraph, "hexBinary")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, hexBinary, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("([0-9a-fA-F]{2})*")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#boolean
      boolean <- addScalarDataType(xsd_mgraph, "boolean")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, boolean, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(finite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#date
      date <- addScalarDataType(xsd_mgraph, "date")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, date, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTime
      dateTime <- addScalarDataType(xsd_mgraph, "dateTime")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, dateTime, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse), explicitTimezone(nonFixedOptional)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTimeStamp
      dateTimeStamp <- addScalarDataType(xsd_mgraph, "dateTimeStamp")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, dateTimeStamp, dateTime,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(whiteSpace(fixedCollapse), explicitTimezone(fixedRequired)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#decimal
      decimal <- addScalarDataType(xsd_mgraph, "decimal")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, decimal, anyAtomicType,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#integer
      integer <- addScalarDataType(xsd_mgraph, "integer")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, integer, decimal,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(fixedFractionDigits(0), whiteSpace(fixedCollapse), pattern("[\\-+]?[0-9]+")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#long
      long <- addScalarDataType(xsd_mgraph, "long")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, long, integer,
        Seq(ordered(total), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("9223372036854775807"),
          nonFixedMinInclusive("-9223372036854775808")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#int
      int <- addScalarDataType(xsd_mgraph, "int")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, int, long,
        Seq(ordered(total), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("2147483647"),
          nonFixedMinInclusive("-2147483648")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#short
      short <- addScalarDataType(xsd_mgraph, "short")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, short, int,
        Seq(ordered(total), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("32767"),
          nonFixedMinInclusive("-32768")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#byte
      byte <- addScalarDataType(xsd_mgraph, "byte")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, byte, short,
        Seq(ordered(total), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("127"),
          nonFixedMinInclusive("-128")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonNegativeInteger
      nonNegativeInteger <- addScalarDataType(xsd_mgraph, "nonNegativeInteger")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, nonNegativeInteger, integer,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMinInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#positiveInteger
      positiveInteger <- addScalarDataType(xsd_mgraph, "positiveInteger")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, positiveInteger, nonNegativeInteger,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMinInclusive("1")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedLong
      unsignedLong <- addScalarDataType(xsd_mgraph, "unsignedLong")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, unsignedLong, nonNegativeInteger,
        Seq(ordered(total), bounded(false), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("18446744073709551615"),
          nonFixedMinInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedInt
      unsignedInt <- addScalarDataType(xsd_mgraph, "unsignedInt")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, unsignedInt, unsignedLong,
        Seq(ordered(total), bounded(false), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("4294967295"),
          nonFixedMinInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedShort
      unsignedShort <- addScalarDataType(xsd_mgraph, "unsignedShort")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, unsignedShort, unsignedInt,
        Seq(ordered(total), bounded(false), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("65535"),
          nonFixedMinInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedByte
      unsignedByte <- addScalarDataType(xsd_mgraph, "unsignedByte")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, unsignedByte, unsignedShort,
        Seq(ordered(total), bounded(false), cardinality(finite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("0"),
          nonFixedMinInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonPositiveInteger
      nonPositiveInteger <- addScalarDataType(xsd_mgraph, "nonPositiveInteger")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, nonPositiveInteger, integer,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("0")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#negativeInteger
      negativeInteger <- addScalarDataType(xsd_mgraph, "negativeInteger")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, negativeInteger, nonPositiveInteger,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(
          fixedFractionDigits(0),
          whiteSpace(fixedCollapse),
          pattern("[\\-+]?[0-9]+"),
          nonFixedMaxInclusive("-1")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#double
      double <- addScalarDataType(xsd_mgraph, "double")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, double, anyAtomicType,
        Seq(ordered(partial), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#float
      float <- addScalarDataType(xsd_mgraph, "float")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, float, anyAtomicType,
        Seq(ordered(partial), bounded(true), cardinality(finite), numeric(true)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#duration
      duration <- addScalarDataType(xsd_mgraph, "duration")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, duration, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("-?P((([0-9]+Y([0-9]+M)?([0-9]+D)?|([0-9]+M)([0-9]+D)?|([0-9]+D))(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S)))?)|(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S))))")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dayTimeDuration
      dayTimeDuration <- addScalarDataType(xsd_mgraph, "dayTimeDuration")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, dayTimeDuration, duration,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("[^YM]*(T.*)?")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#yearMonthDuration
      yearMonthDuration <- addScalarDataType(xsd_mgraph, "yearMonthDuration")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, yearMonthDuration, duration,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          pattern("[^DT]*")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gDay
      gDay <- addScalarDataType(xsd_mgraph, "gDay")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, gDay, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("---(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonth
      gMonth <- addScalarDataType(xsd_mgraph, "gMonth")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, gMonth, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("--(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonthDay
      gMonthDay <- addScalarDataType(xsd_mgraph, "gMonthDay")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, gMonthDay, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("--(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYear
      gYear <- addScalarDataType(xsd_mgraph, "gYear")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, gYear, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("-?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYearMonth
      gYearMonth <- addScalarDataType(xsd_mgraph, "gYearMonth")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, gYearMonth, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is not intended for direct use
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NOTATION

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#QName

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#string
      string <- addScalarDataType(xsd_mgraph, "string")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, string, anyAtomicType,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedPreserve)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#normalizedString
      normalizedString <- addScalarDataType(xsd_mgraph, "normalizedString")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, normalizedString, string,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedReplace)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#token
      token <- addScalarDataType(xsd_mgraph, "token")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, token, normalizedString,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedCollapse)))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#language
      language <- addScalarDataType(xsd_mgraph, "language")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, language, token,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedCollapse),
          pattern("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NMTOKEN
      nmtoken <- addScalarDataType(xsd_mgraph, "NMTOKEN")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, nmtoken, token,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedCollapse),
          pattern("\\c+")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#name
      name <- addScalarDataType(xsd_mgraph, "name")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, name, token,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedCollapse),
          pattern("\\i\\c*")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NCName
      ncname <- addScalarDataType(xsd_mgraph, "NCName")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, ncname, name,
        Seq(ordered(_false), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(nonFixedCollapse),
          pattern("[\\i\\c*&&[\\i-[:]][\\c-[:]]*]")))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#time
      time <- addScalarDataType(xsd_mgraph, "time")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        xsd_mgraph, time, anyAtomicType,
        Seq(ordered(partial), bounded(false), cardinality(countablyInfinite), numeric(false)),
        Seq(
          whiteSpace(fixedCollapse),
          explicitTimezone(nonFixedOptional),
          pattern("(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ENTITY

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ID

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#IDREF

      rdfs_iri <- makeIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns")
      rdfs_mgraph <- makeW3CTerminologyGraphDefinition(rdfs_iri)
      _ <- addTerminologyGraphExtension(rdfs_mgraph, xsd_mgraph)

      // @see http://www.w3.org/TR/rdf11-concepts/#section-html
      // rdf:HTML

      // @see http://www.w3.org/TR/rdf11-concepts/#section-XMLLiteral
      // rdf:XMLLiteral

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl_mgraph <- makeW3CTerminologyGraphDefinition(owl_iri)
      _ <- addTerminologyGraphExtension(owl_mgraph, rdfs_mgraph)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:real
      owl_real <- addScalarDataType(owl_mgraph, "real")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        owl_mgraph, owl_real, anyAtomicType,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(whiteSpace(fixedCollapse)))

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      owl_rational <- addScalarDataType(owl_mgraph, "rational")
      _ <- addScalarDataTypeFacetRestrictionAxiom(
        owl_mgraph, owl_rational, owl_real,
        Seq(ordered(total), bounded(false), cardinality(countablyInfinite), numeric(true)),
        Seq(whiteSpace(fixedCollapse), pattern("[\\-+]?[0-9]+/[1-9][0-9]*")))

      result <- asImmutableTerminologyGraph(owl_mgraph)
    } yield result
  }

}