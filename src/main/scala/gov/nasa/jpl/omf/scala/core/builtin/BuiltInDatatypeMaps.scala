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

import gov.nasa.jpl.omf.scala.core.{OMF, OMFOps}

import scala.collection.immutable.{Iterable,Set}
import scala.{Boolean,Some,Tuple3}
import scalaz.\/

object BuiltInDatatypeMaps {

  case class DataRangeCategories[omf <: OMF]
  ( numeric: Set[omf#DataRange] = Set.empty[omf#DataRange],
    string: Set[omf#DataRange] = Set.empty[omf#DataRange],
    plainLiteral: Set[omf#DataRange] = Set.empty[omf#DataRange],
    binary: Set[omf#DataRange] = Set.empty[omf#DataRange],
    iri: Set[omf#DataRange] = Set.empty[omf#DataRange],
    time: Set[omf#DataRange] = Set.empty[omf#DataRange] ) {

    protected def isCategoryRestriction
    (category: Set[omf#DataRange],
     dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = category.contains(dr) ||
      OMFOps
        .closure[omf#DataRange, omf#DataRange](dr, ops.restrictedDataRangeOf(_).to[Iterable])
        .exists(category.contains)

    def isNumericKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(numeric, dr)

    def withNumeric(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(numeric = this.numeric + dr)

    def isStringKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(string, dr)

    def withString(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(string = this.string + dr)

    def isPlainLiteralKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(plainLiteral, dr)

    def withPlainLiteral(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(plainLiteral = this.plainLiteral + dr)

    def isBinaryKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(binary, dr)

    def withBinary(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(binary = this.binary + dr)

    def isIRIKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(iri, dr)

    def withIRI(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(iri = this.iri + dr)

    def isTimeKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(time, dr)

    def withTime(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(time = this.time + dr)

  }

  def createBuiltInDatatypeMaps[omf <: OMF]
  (makeW3CTerminologyGraphDefinition: omf#IRI => Set[java.lang.Throwable] \/ omf#MutableTerminologyBox)
  (implicit
   ops: OMFOps[omf],
   store: omf#Store)
  : Set[java.lang.Throwable] \/
    ( omf#ImmutableTerminologyBox,
      omf#Mutable2ImmutableTerminologyMap,
      DataRangeCategories[omf] )
  = {
    import ops._

    val dcr0 = DataRangeCategories[omf]()

    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd_mgraph <- makeW3CTerminologyGraphDefinition(xsd_iri)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyAtomicType
      anyAtomicType <- addScalarDataType(
        xsd_mgraph, "anyAtomicType")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyURI
      anyURI <- addScalarDataType(
        xsd_mgraph, "anyURI")
      dcr1 = dcr0.withIRI(anyURI)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#base64Binary
      base64Binary <- addScalarDataType(
        xsd_mgraph, "base64Binary")
      dcr2 = dcr1.withBinary(base64Binary)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#hexBinary
      hexBinary <- addStringScalarRestriction(
        xsd_mgraph, "hexBinary", anyAtomicType, pattern=Some("([0-9a-fA-F]{2})*"))
      dcr3 = dcr2.withBinary(hexBinary)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#boolean
      boolean <- addScalarDataType(
        xsd_mgraph, "boolean")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#date
      date <- addScalarDataType(
        xsd_mgraph, "date")

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTime
      dateTime <- addScalarDataType(
        xsd_mgraph, "dateTime")
      dcr4 = dcr3.withTime(dateTime)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTimeStamp
      dateTimeStamp <- addScalarDataType(
        xsd_mgraph, "dateTimeStamp")
      dcr5 = dcr4.withTime(dateTimeStamp)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#decimal
      decimal <- addScalarDataType(
        xsd_mgraph, "decimal")
      dcr6 = dcr5.withNumeric(decimal)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#integer
      integer <- addStringScalarRestriction(
        xsd_mgraph, "integer", decimal, pattern=Some("[\\-+]?[0-9]+"))
      dcr7 = dcr6.withNumeric(integer)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#long
      long <- addNumericScalarRestriction(
        xsd_mgraph, "long", integer,
        minInclusive=Some("-9223372036854775808"), maxInclusive=Some("9223372036854775807"))
      dcr8 = dcr7.withNumeric(long)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#int
      int <- addNumericScalarRestriction(
        xsd_mgraph, "int", long,
        minInclusive=Some("-2147483648"), maxInclusive=Some("2147483647"))
      dcr9 = dcr8.withNumeric(int)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#short
      short <- addNumericScalarRestriction(
        xsd_mgraph, "short", int,
        minInclusive=Some("-32768"), maxInclusive=Some("32767"))
      dcr10 = dcr9.withNumeric(short)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#byte
      byte <- addNumericScalarRestriction(
        xsd_mgraph, "byte", short,
        minInclusive=Some("-128"), maxInclusive=Some("127"))
      dcr11 = dcr10.withNumeric(byte)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonNegativeInteger
      nonNegativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, "nonNegativeInteger", integer,
        minInclusive=Some("0"))
      dcr12 = dcr11.withNumeric(nonNegativeInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#positiveInteger
      positiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, "positiveInteger", nonNegativeInteger,
        minInclusive=Some("1"))
      dcr13 = dcr12.withNumeric(positiveInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedLong
      unsignedLong <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedLong", nonNegativeInteger,
        maxInclusive=Some("18446744073709551615"))
      dcr14 = dcr13.withNumeric(unsignedLong)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedInt
      unsignedInt <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedInt", unsignedLong,
        maxInclusive=Some("4294967295"))
      dcr15 = dcr14.withNumeric(unsignedInt)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedShort
      unsignedShort <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedShort", unsignedInt,
        maxInclusive=Some("65535"))
      dcr16 = dcr15.withNumeric(unsignedShort)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedByte
      unsignedByte <- addNumericScalarRestriction(
        xsd_mgraph, "unsignedByte", unsignedShort,
        maxInclusive=Some("255"))
      dcr17 = dcr16.withNumeric(unsignedByte)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonPositiveInteger
      nonPositiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, "nonPositiveInteger", integer,
        maxInclusive=Some("0"))
      dcr18 = dcr17.withNumeric(nonPositiveInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#negativeInteger
      negativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, "negativeInteger", nonPositiveInteger,
        maxInclusive=Some("-1"))
      dcr19 = dcr18.withNumeric(negativeInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#double
      double <- addStringScalarRestriction(
        xsd_mgraph, "double", anyAtomicType,
        pattern=Some("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN"))
      dcr20 = dcr19.withNumeric(double)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#float
      float <- addStringScalarRestriction(
        xsd_mgraph, "float", anyAtomicType,
        pattern=Some("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN"))
      dcr21 = dcr20.withNumeric(float)

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
      dcr22 = dcr21.withString(string)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#normalizedString
      normalizedString <- addStringScalarRestriction(
        xsd_mgraph, "normalizedString", string,
        pattern=Some("[^\\n\\r\\t]"))
      dcr23 = dcr22.withString(normalizedString)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#token
      token <- addStringScalarRestriction(
        xsd_mgraph, "token", normalizedString,
        pattern=Some("\\S[\\S[ ]{0,2}]\\S"))
      dcr24 = dcr23.withString(token)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#language
      language <- addStringScalarRestriction(
        xsd_mgraph, "language", token,
        pattern=Some("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"))
      dcr25 = dcr24.withString(language)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NMTOKEN
      nmtoken <- addStringScalarRestriction(
        xsd_mgraph, "NMTOKEN", token,
        pattern=Some("\\c+"))
      dcr26 = dcr25.withString(nmtoken)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#name
      name <- addStringScalarRestriction(
        xsd_mgraph, "name", token,
        pattern=Some("\\i\\c*"))
      dcr27 = dcr26.withString(name)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NCName
      ncname <- addStringScalarRestriction(
        xsd_mgraph, "NCName", name,
        pattern=Some("[\\i\\c*&&[\\i-[:]][\\c-[:]]*]"))
      dcr28 = dcr27.withString(ncname)

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
      xmlLiteral <- addScalarDataType(rdfs_mgraph, "XMLLiteral")

      // @see https://www.w3.org/TR/2012/REC-rdf-plain-literal-20121211/
      plainLiteral <- addScalarDataType(rdfs_mgraph, "PlainLiteral")
      dcr29 = dcr28.withString(plainLiteral)

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl_mgraph <- makeW3CTerminologyGraphDefinition(owl_iri)
      _ <- addTerminologyExtension(owl_mgraph, rdfs_mgraph)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:real
      owl_real <- addStringScalarRestriction(owl_mgraph, "real", anyAtomicType)
      dcr30 = dcr29.withNumeric(owl_real)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      owl_rational <- addStringScalarRestriction(
        owl_mgraph, "rational", owl_real,
        pattern=Some("[\\-+]?[0-9]+/[1-9][0-9]*"))
      dcr31 = dcr30.withNumeric(owl_rational)

      result <- asImmutableTerminology(owl_mgraph)
    } yield Tuple3(result._1, result._2, dcr31)
  }

}