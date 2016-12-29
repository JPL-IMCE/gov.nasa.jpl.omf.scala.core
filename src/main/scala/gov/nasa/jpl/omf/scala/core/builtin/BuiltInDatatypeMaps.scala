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

package gov.nasa.jpl.omf.scala.core.builtin

import gov.nasa.jpl.omf.scala.core.{OMFOps, OMF}

import scala.collection.immutable._
import scala.Some
import scalaz.\/

object BuiltInDatatypeMaps {

  def createBuiltInDatatypeMaps[omf <: OMF]
  (makeW3CTerminologyGraphDefinition: omf#IRI => Set[java.lang.Throwable] \/ omf#MutableTerminologyBox)
  (implicit
   ops: OMFOps[omf],
   store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableTerminologyBox, omf#Mutable2ImmutableTerminologyMap)
  = {
    import ops._

    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd_mgraph <- makeW3CTerminologyGraphDefinition(xsd_iri)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyAtomicType
      anyAtomicType <- addScalarDataType(
        xsd_mgraph, "anyAtomicType")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyURI
      anyURI <- addScalarDataType(
        xsd_mgraph, "anyURI")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#base64Binary
      base64Binary <- addScalarDataType(
        xsd_mgraph, "base64Binary")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#hexBinary
      hexBinary <- addStringScalarRestriction(
        xsd_mgraph, "hexBinary", anyAtomicType, pattern=Some("([0-9a-fA-F]{2})*"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#boolean
      boolean <- addScalarDataType(
        xsd_mgraph, "boolean")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#date
      date <- addScalarDataType(
        xsd_mgraph, "date")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTime
      dateTime <- addScalarDataType(
        xsd_mgraph, "dateTime")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTimeStamp
      dateTimeStamp <- addScalarDataType(
        xsd_mgraph, "dateTimeStamp")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#decimal
      decimal <- addScalarDataType(
        xsd_mgraph, "decimal")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#integer
      integer <- addStringScalarRestriction(
        xsd_mgraph, "integer", decimal, pattern=Some("[\\-+]?[0-9]+"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#long
      long <- addNumericScalarRestriction(
        xsd_mgraph, "long", integer,
        minInclusive=Some("-9223372036854775808"), maxInclusive=Some("9223372036854775807"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#int
      int <- addNumericScalarRestriction(
        xsd_mgraph, "int", long,
        minInclusive=Some("-2147483648"), maxInclusive=Some("2147483647"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#short
      short <- addNumericScalarRestriction(
        xsd_mgraph, "short", int,
        minInclusive=Some("-32768"), maxInclusive=Some("32767"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#byte
      byte <- addNumericScalarRestriction(
        xsd_mgraph, "byte", short,
        minInclusive=Some("-128"), maxInclusive=Some("127"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonNegativeInteger
      nonNegativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, "nonNegativeInteger", integer,
        minInclusive=Some("0"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#positiveInteger
      positiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, "positiveInteger", nonNegativeInteger,
        minInclusive=Some("1"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedLong
      unsignedLong <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedLong", nonNegativeInteger,
        maxInclusive=Some("18446744073709551615"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedInt
      unsignedInt <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedInt", unsignedLong,
        maxInclusive=Some("4294967295"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedShort
      unsignedShort <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedShort", unsignedInt,
        maxInclusive=Some("65535"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedByte
      unsignedByte <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedByte", unsignedShort,
        maxInclusive=Some("255"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonPositiveInteger
      nonPositiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, "nonPositiveInteger", integer,
        maxInclusive=Some("0"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#negativeInteger
      negativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, "negativeInteger", nonPositiveInteger,
        maxInclusive=Some("-1"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#double
      double <- addStringScalarRestriction(
        xsd_mgraph, "double", anyAtomicType,
        pattern=Some("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#float
      float <- addStringScalarRestriction(
        xsd_mgraph, "float", anyAtomicType,
        pattern=Some("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#duration
      duration <- addStringScalarRestriction(
        xsd_mgraph, "duration", anyAtomicType,
        pattern=Some("-?P((([0-9]+Y([0-9]+M)?([0-9]+D)?|([0-9]+M)([0-9]+D)?|([0-9]+D))(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S)))?)|(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S))))"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dayTimeDuration
      dayTimeDuration <- addStringScalarRestriction(
        xsd_mgraph, "dayTimeDuration", duration,
        pattern=Some("[^YM]*(T.*)?"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#yearMonthDuration
      yearMonthDuration <- addStringScalarRestriction(
        xsd_mgraph, "yearMonthDuration", duration,
        pattern=Some("[^DT]*"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gDay
      gDay <- addStringScalarRestriction(
        xsd_mgraph, "gDay", anyAtomicType,
        pattern=Some("---(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonth
      gMonth <- addStringScalarRestriction(
        xsd_mgraph, "gMonth", anyAtomicType,
        pattern=Some("--(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonthDay
      gMonthDay <- addStringScalarRestriction(
        xsd_mgraph, "gMonthDay", anyAtomicType,
        pattern=Some("--(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYear
      gYear <- addStringScalarRestriction(
        xsd_mgraph, "gYear", anyAtomicType,
        pattern=Some("-?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYearMonth
      gYearMonth <- addStringScalarRestriction(
        xsd_mgraph, "gYearMonth", anyAtomicType,
        pattern=Some("-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is not intended for direct use
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NOTATION

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#QName

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#string
      string <- addScalarDataType(xsd_mgraph, "string")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#normalizedString
      normalizedString <- addStringScalarRestriction(
        xsd_mgraph, "normalizedString", string,
        pattern=Some("[^\\n\\r\\t]"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#token
      token <- addStringScalarRestriction(
        xsd_mgraph, "token", normalizedString,
        pattern=Some("\\S[\\S[ ]{0,2}]\\S"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#language
      language <- addStringScalarRestriction(
        xsd_mgraph, "language", token,
        pattern=Some("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NMTOKEN
      nmtoken <- addStringScalarRestriction(
        xsd_mgraph, "NMTOKEN", token,
        pattern=Some("\\c+"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#name
      name <- addStringScalarRestriction(
        xsd_mgraph, "name", token,
        pattern=Some("\\i\\c*"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NCName
      ncname <- addStringScalarRestriction(
        xsd_mgraph, "NCName", name,
        pattern=Some("[\\i\\c*&&[\\i-[:]][\\c-[:]]*]"))

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#time
      time <- addStringScalarRestriction(
        xsd_mgraph, "time", anyAtomicType,
        pattern=Some("(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?"))

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
      _ <- addTerminologyExtension(rdfs_mgraph, xsd_mgraph)

      // @see http://www.w3.org/TR/rdf11-concepts/#section-html
      // rdf:HTML

      // @see http://www.w3.org/TR/rdf11-concepts/#section-XMLLiteral
      // rdf:XMLLiteral

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl_mgraph <- makeW3CTerminologyGraphDefinition(owl_iri)
      _ <- addTerminologyExtension(owl_mgraph, rdfs_mgraph)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:real
      owl_real <- addStringScalarRestriction(owl_mgraph, "real", anyAtomicType)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      owl_rational <- addStringScalarRestriction(
        owl_mgraph, "rational", owl_real,
        pattern=Some("[\\-+]?[0-9]+/[1-9][0-9]*"))

      result <- asImmutableTerminology(owl_mgraph)
    } yield result
  }

}