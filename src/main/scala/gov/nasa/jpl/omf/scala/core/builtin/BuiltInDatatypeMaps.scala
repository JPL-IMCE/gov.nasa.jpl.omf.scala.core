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

import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{Mutable2ImmutableModuleTable, OMF, OMFOps}
import gov.nasa.jpl.imce.oml.tables.taggedTypes.{abbrevIRI, iri, localName,literalPattern}
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty,LiteralDecimalType,LiteralNumber,LiteralPositiveIntegerType}
import gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator

import scala.collection.immutable.{Iterable, Seq, Set}
import scala.{Boolean, None, Option, Some}
import scalaz._
import Scalaz._

object BuiltInDatatypeMaps {

  case class DataRangeCategories[omf <: OMF]
  ( builtInImport: Option[omf#TerminologyBox] = None,
    builtInDatatypeModules: Set[omf#Module] = Set.empty[omf#Module],
    anyAtomicType: Option[omf#DataRange] = None,
    boolean: Option[omf#DataRange] = None,
    numeric: Set[omf#DataRange] = Set.empty[omf#DataRange],
    string: Set[omf#DataRange] = Set.empty[omf#DataRange],
    plainLiteral: Set[omf#DataRange] = Set.empty[omf#DataRange],
    xmlLiteral: Set[omf#DataRange] = Set.empty[omf#DataRange],
    binary: Set[omf#DataRange] = Set.empty[omf#DataRange],
    iri: Set[omf#DataRange] = Set.empty[omf#DataRange],
    time: Set[omf#DataRange] = Set.empty[omf#DataRange],
    nonNormative: Set[omf#DataRange] = Set.empty[omf#DataRange]) {

    def lookupBuiltInModule
    (iri: omf#IRI)
    (implicit ops: OMFOps[omf])
    : Option[omf#Module]
    = builtInDatatypeModules.find { m => ops.getModuleIRI(m) == iri }

    def isBuiltInModule
    (iri: omf#IRI)
    (implicit ops: OMFOps[omf])
    : Boolean
    = lookupBuiltInModule(iri).nonEmpty

    def dataRanges
    : Set[omf#DataRange]
    = anyAtomicType.to[Set] ++
    boolean.to[Set] ++
    numeric ++
    string ++
    plainLiteral ++
    xmlLiteral ++
    binary ++
    iri ++
    time ++
    nonNormative

    protected def isCategoryRestriction
    (category: Set[omf#DataRange],
     dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = category.contains(dr) ||
      OMFOps
        .closure[omf#DataRange, omf#DataRange](dr, ops.restrictedDataRangeOf(_).to[Iterable])
        .exists(category.contains)

    def lookupBuiltInDataRange
    (drIRI: omf#IRI)
    (implicit ops: OMFOps[omf])
    : Option[omf#DataRange]
    = anyAtomicType.find { ops.getTermIRI(_) == drIRI } orElse
      boolean.find { ops.getTermIRI(_) == drIRI } orElse
      numeric.find { ops.getTermIRI(_) == drIRI } orElse
      string.find { ops.getTermIRI(_) == drIRI } orElse
      plainLiteral.find { ops.getTermIRI(_) == drIRI } orElse
      xmlLiteral.find { ops.getTermIRI(_) == drIRI } orElse
      binary.find { ops.getTermIRI(_) == drIRI } orElse
      iri.find { ops.getTermIRI(_) == drIRI } orElse
      time.find { ops.getTermIRI(_) == drIRI } orElse
      nonNormative.find { ops.getTermIRI(_) == drIRI }

    def withBuiltInImport(tbox: omf#TerminologyBox)
    : DataRangeCategories[omf]
    = copy(builtInImport = Some(tbox), builtInDatatypeModules = builtInDatatypeModules + tbox)

    def withBuiltInDatatypeModule(m: omf#Module)
    : DataRangeCategories[omf]
    = copy(builtInDatatypeModules = builtInDatatypeModules + m)

    def withAnyAtomicType(any: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(anyAtomicType = Some(any))

    def isAnyAtomicType
    (dr: omf#DataRange)
    : Boolean
    = anyAtomicType.contains(dr)

    def withBoolean(bool: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(boolean = Some(bool))

    def isBoolean
    (dr: omf#DataRange)
    : Boolean
    = boolean.contains(dr)

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

    def isXMLLiteralKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(xmlLiteral, dr)

    def withXMLLiteral(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(xmlLiteral = this.xmlLiteral + dr)

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

    def isNonNormativeKind
    (dr: omf#DataRange)
    (implicit ops: OMFOps[omf], store: omf#Store)
    : Boolean
    = isCategoryRestriction(nonNormative, dr)

    def withNonNormative(dr: omf#DataRange)
    : DataRangeCategories[omf]
    = copy(nonNormative = this.nonNormative + dr)

  }

  def resolveBuiltInDatatypeMaps[omf <: OMF]
  (m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ DataRangeCategories[omf]
  = {
    import ops._

    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd <- m2i.getImmutableTerminologyGraph(xsd_iri)

      anyAtomicType <- getDataRange(xsd, localName("anyAtomicType"))
      dcr0 = DataRangeCategories[omf]().withAnyAtomicType(anyAtomicType)

      anyURI <- getDataRange(xsd, localName("anyURI"))
      dcr1 = dcr0.withIRI(anyURI)

      base64Binary <- getDataRange(xsd, localName("base64Binary"))
      dcr2 = dcr1.withBinary(base64Binary)

      hexBinary <- getDataRange(xsd, localName("hexBinary"))
      dcr3 = dcr2.withBinary(hexBinary)

      boolean <- getDataRange(xsd, localName("boolean"))
      dcr4 = dcr3.withBoolean(boolean)

      date <- getDataRange(xsd, localName("date"))
      dcr5 = dcr4.withNonNormative(date)

      dateTime <- getDataRange(xsd, localName("dateTime"))
      dcr6 = dcr5.withTime(dateTime)

      dateTimeStamp <- getDataRange(xsd, localName("dateTimeStamp"))
      dcr7 = dcr6.withTime(dateTimeStamp)

      decimal <- getDataRange(xsd, localName("decimal"))
      dcr8 = dcr7.withNumeric(decimal)

      integer <- getDataRange(xsd, localName("integer"))
      dcr9 = dcr8.withNumeric(integer)

      long <- getDataRange(xsd, localName("long"))
      dcr10 = dcr9.withNumeric(long)

      int <- getDataRange(xsd, localName("int"))
      dcr11 = dcr10.withNumeric(int)

      short <- getDataRange(xsd, localName("short"))
      dcr12 = dcr11.withNumeric(short)

      byte <- getDataRange(xsd, localName("byte"))
      dcr13 = dcr12.withNumeric(byte)

      nonNegativeInteger <- getDataRange(xsd, localName("nonNegativeInteger"))
      dcr14 = dcr13.withNumeric(nonNegativeInteger)

      positiveInteger <- getDataRange(xsd, localName("positiveInteger"))
      dcr15 = dcr14.withNumeric(positiveInteger)

      unsignedLong <- getDataRange(xsd, localName("unsignedLong"))
      dcr16 = dcr15.withNumeric(unsignedLong)

      unsignedInt <- getDataRange(xsd, localName("unsignedInt"))
      dcr17 = dcr16.withNumeric(unsignedInt)

      unsignedShort <- getDataRange(xsd, localName("unsignedShort"))
      dcr18 = dcr17.withNumeric(unsignedShort)

      unsignedByte <- getDataRange(xsd, localName("unsignedByte"))
      dcr19 = dcr18.withNumeric(unsignedByte)

      nonPositiveInteger <- getDataRange(xsd, localName("nonPositiveInteger"))
      dcr20 = dcr19.withNumeric(nonPositiveInteger)

      negativeInteger <- getDataRange(xsd, localName("negativeInteger"))
      dcr21 = dcr20.withNumeric(negativeInteger)

      double <- getDataRange(xsd, localName("double"))
      dcr22 = dcr21.withNumeric(double)

      float <- getDataRange(xsd, localName("float"))
      dcr23 = dcr22.withNumeric(float)

      duration <- getDataRange(xsd, localName("duration"))
      dcr24 = dcr23.withNonNormative(duration)

      dayTimeDuration <- getDataRange(xsd, localName("dayTimeDuration"))
      dcr25 = dcr24.withNonNormative(dayTimeDuration)

      yearMonthDuration <- getDataRange(xsd, localName("yearMonthDuration"))
      dcr26 = dcr25.withNonNormative(yearMonthDuration)

      gDay <- getDataRange(xsd, localName("gDay"))
      dcr27 = dcr26.withNonNormative(gDay)

      gMonth <- getDataRange(xsd, localName("gMonth"))
      dcr28 = dcr27.withNonNormative(gMonth)

      gMonthDay <- getDataRange(xsd, localName("gMonthDay"))
      dcr29 = dcr28.withNonNormative(gMonthDay)

      gYear <- getDataRange(xsd, localName("gYear"))
      dcr30 = dcr29.withNonNormative(gYear)

      gYearMonth <- getDataRange(xsd, localName("gYearMonth"))
      dcr31 = dcr30.withNonNormative(gYearMonth)

      string <- getDataRange(xsd, localName("string"))
      dcr32 = dcr31.withString(string)

      normalizedString <- getDataRange(xsd, localName("normalizedString"))
      dcr33 = dcr32.withString(normalizedString)

      token <- getDataRange(xsd, localName("token"))
      dcr34 = dcr33.withString(token)

      language <- getDataRange(xsd, localName("language"))
      dcr35 = dcr34.withString(language)

      nmtoken <- getDataRange(xsd, localName("NMTOKEN"))
      dcr36 = dcr35.withString(nmtoken)

      name <- getDataRange(xsd, localName("name"))
      dcr37 = dcr36.withString(name)

      ncname <- getDataRange(xsd, localName("NCName"))
      dcr38 = dcr37.withString(ncname)

      time <- getDataRange(xsd, localName("time"))
      dcr39 = dcr38.withTime(time)

      rdfs_iri <- makeIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns")
      rdfs <- m2i.getImmutableTerminologyGraph(rdfs_iri)

      xmlLiteral <- getDataRange(xsd, localName("XMLLiteral"))
      dcr40 = dcr39.withXMLLiteral(xmlLiteral)

      plainLiteral <- getDataRange(xsd, localName("PlainLiteral"))
      dcr41 = dcr40.withPlainLiteral(plainLiteral)

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl <- m2i.getImmutableTerminologyGraph(owl_iri)

      owl_real <- getDataRange(xsd, localName("real"))
      dcr42 = dcr41.withNumeric(owl_real)

      owl_rational <- getDataRange(xsd, localName("rational"))
      dcr43 = dcr42.withNumeric(owl_rational)

      dcr = dcr43
        .withBuiltInImport(owl)
        .withBuiltInDatatypeModule(xsd)
        .withBuiltInDatatypeModule(rdfs)

    } yield dcr
  }

  def createBuiltInDatatypeMaps[omf <: OMF]
  (makeW3CTerminologyGraphDefinition: omf#IRI => Throwables \/ omf#MutableTerminologyBox)
  (implicit
   ops: OMFOps[omf],
   store: omf#Store)
  : Throwables \/ DataRangeCategories[omf]
  = {
    import ops._

    import gov.nasa.jpl.imce.oml.resolver.toUUIDString

    val uuidGen = JVMUUIDGenerator()

    for {
      xsd_iri <- makeIRI("http://www.w3.org/2001/XMLSchema")
      xsd_mgraph <- makeW3CTerminologyGraphDefinition(xsd_iri)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyAtomicType
      anyAtomicType <- addScalarDataType(
        xsd_mgraph, localName("anyAtomicType"))
      dcr0 = DataRangeCategories[omf]().withAnyAtomicType(anyAtomicType)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#anyURI
      anyURI <- addScalarDataType(
        xsd_mgraph, localName("anyURI"))
      dcr1 = dcr0.withIRI(anyURI)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#base64Binary
      base64Binary <- addScalarDataType(
        xsd_mgraph, localName("base64Binary"))
      dcr2 = dcr1.withBinary(base64Binary)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#hexBinary
      hexBinary <- addStringScalarRestriction(
        xsd_mgraph, localName("hexBinary"), anyAtomicType, pattern=Some(literalPattern("([0-9a-fA-F]{2})*")))
      dcr3 = dcr2.withBinary(hexBinary)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#boolean
      boolean <- addScalarDataType(
        xsd_mgraph, localName("boolean"))
      dcr4 = dcr3.withBoolean(boolean)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#date
      date <- addScalarDataType(
        xsd_mgraph, localName("date"))
      dcr5 = dcr4.withNonNormative(date)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTime
      dateTime <- addScalarDataType(
        xsd_mgraph, localName("dateTime"))
      dcr6 = dcr5.withTime(dateTime)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dateTimeStamp
      dateTimeStamp <- addScalarDataType(
        xsd_mgraph, localName("dateTimeStamp"))
      dcr7 = dcr6.withTime(dateTimeStamp)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#decimal
      decimal <- addScalarDataType(
        xsd_mgraph, localName("decimal"))
      dcr8 = dcr7.withNumeric(decimal)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#integer
      integer <- addStringScalarRestriction(
        xsd_mgraph, localName("integer"), decimal,
        pattern=Some(literalPattern("[\\-+]?[0-9]+")))
      dcr9 = dcr8.withNumeric(integer)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#long
      long <- addNumericScalarRestriction(
        xsd_mgraph, localName("long"), integer,
        minInclusive=Some(LiteralNumber(LiteralDecimalType,"-9223372036854775808")),
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType, "9223372036854775807")))
      dcr10 = dcr9.withNumeric(long)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#int
      int <- addNumericScalarRestriction(
        xsd_mgraph, localName("int"), long,
        minInclusive=Some(LiteralNumber(LiteralDecimalType,"-2147483648")),
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"2147483647")))
      dcr11 = dcr10.withNumeric(int)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#short
      short <- addNumericScalarRestriction(
        xsd_mgraph, localName("short"), int,
        minInclusive=Some(LiteralNumber(LiteralDecimalType,"-32768")),
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"32767")))
      dcr12 = dcr11.withNumeric(short)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#byte
      byte <- addNumericScalarRestriction(
        xsd_mgraph, localName("byte"), short,
        minInclusive=Some(LiteralNumber(LiteralDecimalType,"-128")),
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"127")))
      dcr13 = dcr12.withNumeric(byte)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonNegativeInteger
      nonNegativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, localName("nonNegativeInteger"), integer,
        minInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"0")))
      dcr14 = dcr13.withNumeric(nonNegativeInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#positiveInteger
      positiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, localName("positiveInteger"), nonNegativeInteger,
        minInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"1")))
      dcr15 = dcr14.withNumeric(positiveInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedLong
      unsignedLong <- addNumericScalarRestriction(
        xsd_mgraph, localName("unsignedLong"), nonNegativeInteger,
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"18446744073709551615")))
      dcr16 = dcr15.withNumeric(unsignedLong)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedInt
      unsignedInt <- addNumericScalarRestriction(
        xsd_mgraph, localName("unsignedInt"), unsignedLong,
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"4294967295")))
      dcr17 = dcr16.withNumeric(unsignedInt)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedShort
      unsignedShort <- addNumericScalarRestriction(
        xsd_mgraph, localName("unsignedShort"), unsignedInt,
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"65535")))
      dcr18 = dcr17.withNumeric(unsignedShort)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#unsignedByte
      unsignedByte <- addNumericScalarRestriction(
        xsd_mgraph, localName("unsignedByte"), unsignedShort,
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"255")))
      dcr19 = dcr18.withNumeric(unsignedByte)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#nonPositiveInteger
      nonPositiveInteger <- addNumericScalarRestriction(
        xsd_mgraph, localName("nonPositiveInteger"), integer,
        maxInclusive=Some(LiteralNumber(LiteralPositiveIntegerType,"0")))
      dcr20 = dcr19.withNumeric(nonPositiveInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#negativeInteger
      negativeInteger <- addNumericScalarRestriction(
        xsd_mgraph, localName("negativeInteger"), nonPositiveInteger,
        maxInclusive=Some(LiteralNumber(LiteralDecimalType,"-1")))
      dcr21 = dcr20.withNumeric(negativeInteger)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#double
      double <- addStringScalarRestriction(
        xsd_mgraph, localName("double"), anyAtomicType,
        pattern=Some(literalPattern("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN")))
      dcr22 = dcr21.withNumeric(double)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#float
      float <- addStringScalarRestriction(
        xsd_mgraph, localName("float"), anyAtomicType,
        pattern=Some(literalPattern("(\\+|\\-)?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([Ee](\\+|\\-)?[0-9]+)?|(\\+|\\-)?INF|NaN")))
      dcr23 = dcr22.withNumeric(float)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#duration
      duration <- addStringScalarRestriction(
        xsd_mgraph, localName("duration"), anyAtomicType,
        pattern=Some(literalPattern("-?P((([0-9]+Y([0-9]+M)?([0-9]+D)?|([0-9]+M)([0-9]+D)?|([0-9]+D))(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S)))?)|(T(([0-9]+H)([0-9]+M)?([0-9]+(\\.[0-9]+)?S)?|([0-9]+M)([0-9]+(\\.[0-9]+)?S)?|([0-9]+(\\.[0-9]+)?S))))")))
      dcr24 = dcr23.withNonNormative(duration)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#dayTimeDuration
      dayTimeDuration <- addStringScalarRestriction(
        xsd_mgraph, localName("dayTimeDuration"), duration,
        pattern=Some(literalPattern("[^YM]*(T.*)?")))
      dcr25 = dcr24.withNonNormative(dayTimeDuration)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#yearMonthDuration
      yearMonthDuration <- addStringScalarRestriction(
        xsd_mgraph, localName("yearMonthDuration"), duration,
        pattern=Some(literalPattern("[^DT]*")))
      dcr26 = dcr25.withNonNormative(yearMonthDuration)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gDay
      gDay <- addStringScalarRestriction(
        xsd_mgraph, localName("gDay"), anyAtomicType,
        pattern=Some(literalPattern("---(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr27 = dcr26.withNonNormative(gDay)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonth
      gMonth <- addStringScalarRestriction(
        xsd_mgraph, localName("gMonth"), anyAtomicType,
        pattern=Some(literalPattern("--(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr28 = dcr27.withNonNormative(gMonth)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gMonthDay
      gMonthDay <- addStringScalarRestriction(
        xsd_mgraph, localName("gMonthDay"), anyAtomicType,
        pattern=Some(literalPattern("--(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr29 = dcr28.withNonNormative(gMonthDay)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYear
      gYear <- addStringScalarRestriction(
        xsd_mgraph, localName("gYear"), anyAtomicType,
        pattern=Some(literalPattern("-?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr30 = dcr29.withNonNormative(gYear)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#gYearMonth
      gYearMonth <- addStringScalarRestriction(
        xsd_mgraph, localName("gYearMonth"), anyAtomicType,
        pattern=Some(literalPattern("-?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr31 = dcr30.withNonNormative(gYearMonth)

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is not intended for direct use
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NOTATION

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#QName

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#string
      string <- addScalarDataType(xsd_mgraph, localName("string"))
      dcr32 = dcr31.withString(string)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#normalizedString
      normalizedString <- addStringScalarRestriction(
        xsd_mgraph, localName("normalizedString"), string,
        pattern=Some(literalPattern("[^\\n\\r\\t]")))
      dcr33 = dcr32.withString(normalizedString)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#token
      token <- addStringScalarRestriction(
        xsd_mgraph, localName("token"), normalizedString,
        pattern=Some(literalPattern("\\S[\\S[ ]{0,2}]\\S")))
      dcr34 = dcr33.withString(token)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#language
      language <- addStringScalarRestriction(
        xsd_mgraph, localName("language"), token,
        pattern=Some(literalPattern("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")))
      dcr35 = dcr34.withString(language)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NMTOKEN
      nmtoken <- addStringScalarRestriction(
        xsd_mgraph, localName("NMTOKEN"), token,
        pattern=Some(literalPattern("\\c+")))
      dcr36 = dcr35.withString(nmtoken)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#name
      name <- addStringScalarRestriction(
        xsd_mgraph, localName("name"), token,
        pattern=Some(literalPattern("\\i\\c*")))
      dcr37 = dcr36.withString(name)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#NCName
      ncname <- addStringScalarRestriction(
        xsd_mgraph, localName("NCName"), name,
        pattern=Some(literalPattern("[\\i\\c*&&[\\i-[:]][\\c-[:]]*]")))
      dcr38 = dcr37.withString(ncname)

      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#time
      time <- addStringScalarRestriction(
        xsd_mgraph, localName("time"), anyAtomicType,
        pattern=Some(literalPattern("(([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?")))
      dcr39 = dcr38.withTime(time)

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: requires an enclosing XML document context
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ENTITY

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#ID

      // @see http://www.w3.org/TR/rdf11-concepts/#xsd-datatypes
      // should not be used: this is intended for cross-references within an XML document
      // @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#IDREF

      rdf_iri <- makeIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns")
      rdf_mgraph <- makeW3CTerminologyGraphDefinition(rdf_iri)
      _ <- addTerminologyExtension(rdf_mgraph, xsd_mgraph)

      about <- addTerminologyAnnotationProperty(
        rdf_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(rdf_mgraph),
          iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#about"),
          abbrevIRI("rdf:about")))

      // @see http://www.w3.org/TR/rdf11-concepts/#section-html
      // rdf:HTML

      // @see http://www.w3.org/TR/rdf11-concepts/#section-XMLLiteral
      xmlLiteral <- addScalarDataType(rdf_mgraph, localName("XMLLiteral"))
      dcr40 = dcr39.withXMLLiteral(xmlLiteral)

      // @see https://www.w3.org/TR/2012/REC-rdf-plain-literal-20121211/
      plainLiteral <- addScalarDataType(rdf_mgraph, localName("PlainLiteral"))
      dcr41 = dcr40.withPlainLiteral(plainLiteral)

      owl_iri <- makeIRI("http://www.w3.org/2002/07/owl")
      owl_mgraph <- makeW3CTerminologyGraphDefinition(owl_iri)
      _ <- addTerminologyExtension(owl_mgraph, rdf_mgraph)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      // owl:real
      owl_real <- addStringScalarRestriction(owl_mgraph, localName("real"), anyAtomicType)
      dcr42 = dcr41.withNumeric(owl_real)

      // @see http://www.w3.org/TR/owl2-syntax/#Datatype_Maps
      owl_rational <- addStringScalarRestriction(
        owl_mgraph, localName("rational"), owl_real,
        pattern=Some(literalPattern("[\\-+]?[0-9]+/[1-9][0-9]*")))
      dcr43 = dcr42.withNumeric(owl_rational)

      _ <- addTerminologyAnnotationProperty(
        owl_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(owl_mgraph),
          abbrevIRI = abbrevIRI("owl:backwardCompatibleWith"),
          iri = iri("http://www.w3.org/2002/07/owl#backwardCompatibleWith")))

      _ <- addTerminologyAnnotationProperty(
        owl_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(owl_mgraph),
          abbrevIRI = abbrevIRI("owl:incompatibleWith"),
          iri = iri("http://www.w3.org/2002/07/owl#incompatibleWith")))

      _ <- addTerminologyAnnotationProperty(
        owl_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(owl_mgraph),
          abbrevIRI = abbrevIRI("owl:deprecated"),
          iri = iri("http://www.w3.org/2002/07/owl#deprecated")))

      _ <- addTerminologyAnnotationProperty(
        owl_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(owl_mgraph),
          abbrevIRI = abbrevIRI("owl:priorVersion"),
          iri = iri("http://www.w3.org/2002/07/owl#priorVersion")))

      _ <- addTerminologyAnnotationProperty(
        owl_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(owl_mgraph),
          abbrevIRI = abbrevIRI("owl:versionInfo"),
          iri = iri("http://www.w3.org/2002/07/owl#versionInfo")))

      rdfs_iri <- makeIRI("http://www.w3.org/2000/01/rdf-schema")
      rdfs_mgraph <- makeW3CTerminologyGraphDefinition(rdfs_iri)

      _ <- addTerminologyAnnotationProperty(
        rdfs_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(rdfs_mgraph),
          abbrevIRI = abbrevIRI("rdfs:isDefinedBy"),
          iri = iri("http://www.w3.org/2000/01/rdf-schema#isDefinedBy")))

      _ <- addTerminologyAnnotationProperty(
        rdfs_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(rdfs_mgraph),
          abbrevIRI = abbrevIRI("rdfs:comment"),
          iri = iri("http://www.w3.org/2000/01/rdf-schema#comment")))

      _ <- addTerminologyAnnotationProperty(
        rdfs_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(rdfs_mgraph),
          abbrevIRI = abbrevIRI("rdfs:seeAlso"),
          iri = iri("http://www.w3.org/2000/01/rdf-schema#seeAlso")))

      _ <- addTerminologyAnnotationProperty(
        rdfs_mgraph,
        new AnnotationProperty(
          uuidGen,
          getModuleUUID(rdfs_mgraph),
          abbrevIRI = abbrevIRI("rdfs:label"),
          iri = iri("http://www.w3.org/2000/01/rdf-schema#label")))

      _ <- addTerminologyExtension(owl_mgraph, rdfs_mgraph)

      dc_iri <- makeIRI("http://purl.org/dc/elements/1.1/")
      dc_mgraph <- makeW3CTerminologyGraphDefinition(dc_iri)

      _ <- Seq(
        "contributor",
        "coverage",
        "creator",
        "date",
        "description",
        "format",
        "identifier",
        "language",
        "publisher",
        "relation",
        "rights",
        "source",
        "subject",
        "title",
        "type").foldLeft(().right[Throwables]) { case (acc, name) =>
        acc.flatMap { _ =>
          addTerminologyAnnotationProperty(
            dc_mgraph,
            new AnnotationProperty(
              uuidGen,
              getModuleUUID(dc_mgraph),
              abbrevIRI = abbrevIRI("dc:" + name),
              iri = iri("http://purl.org/dc/elements/1.1/" + name)))
            .map(_ => ())
        }
      }

      dcr = dcr43
        .withBuiltInImport(owl_mgraph)
        .withBuiltInDatatypeModule(rdf_mgraph)
        .withBuiltInDatatypeModule(xsd_mgraph)
        .withBuiltInDatatypeModule(rdfs_mgraph)
        .withBuiltInDatatypeModule(dc_mgraph)

    } yield dcr
  }

}