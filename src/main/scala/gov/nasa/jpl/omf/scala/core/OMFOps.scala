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

package gov.nasa.jpl.omf.scala.core

import java.io.File
import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue, LiteralDateTime, LiteralNumber, LiteralPattern, LiteralValue, PositiveIntegerLiteral}
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.OMLString._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps.DataRangeCategories

import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String}
import scala.collection.immutable.{Iterable, Seq, Set}
import scalaz._
import Scalaz._
import scala.reflect.ClassTag

object OMFOps {

  def apply[omf <: OMF](implicit ops: OMFOps[omf])
  : OMFOps[omf] = ops

  /**
    * @todo a stream-based closure method
    * @param x        initial object of type U
    * @param relation a function from U to V
    * @tparam U
    * @tparam V
    * @return the closure, f(x)+, i.e., f(x), f(f(x)), f(f(f(x))), ...
    */
  def closure[U, V <: U]
  (x: U, relation: U => Iterable[V])
  : Set[V] = {

    case class RelationClosureVisitor
    (result: scala.collection.mutable.Set[V],
     visit: scala.collection.mutable.Buffer[V],
     visited: scala.collection.mutable.Set[V])

    val visitor = RelationClosureVisitor(
      scala.collection.mutable.Set[V](),
      relation(x).toBuffer,
      scala.collection.mutable.Set[V]())

    while (visitor.visit.nonEmpty) {
      val y = visitor.visit.remove(0)
      visitor.visited += y
      visitor.result += y
      relation(y) foreach (yi => {
        visitor.result += yi
        if (!visitor.visited.contains(yi)) {
          visitor.visit += yi
        }
      })
    }
    visitor.result.toSet

  }

}

trait IRIOps[omf <: OMF] {

  // IRI

  def makeIRI(s: String)
  : Throwables \/ omf#IRI

  def getFragment(iri: omf#IRI)
  : Throwables \/ String

  def withFragment(iri: omf#IRI, fragment: LocalName)
  : Throwables \/ omf#IRI

  /**
    * Split the IRI in two components: the IRI without the fragment, the IRI fragment
    */
  def splitIRI(iri: omf#IRI)
  : (omf#IRI, Option[LocalName])

  /**
    * If the IRI has a fragment, returns "n:f" where "n" is the last segment of the IRI and "f" is the fragment of the IRI
    */
  def toAbbreviatedName(iri: omf#IRI, lowercaseFragmentInitial: Boolean)
  : Option[AbbrevIRI]

  /**
    * Extract the last segment of a fragment-less IRI
    *
    * @param iri An IRI without a fragment
    * @return The last segment of the IRI (i.e., the name after the last '/')
    */
  def lastSegment(iri: omf#IRI)
  : Throwables \/ LocalName

  def fromIRI(iri: omf#IRI)
  : String

  /**
    * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
    * @return true if <host> == imce.jpl.nasa.gov and <path> starts with /backbone
    */
  def isBackboneIRI(iri: omf#IRI)
  : Boolean

  /**
    * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
    * @return a new IRI of the form: <scheme><userInfo><host'><port><path'><query><fragment>
    *         where:
    *         <host'> = imce.jpl.nasa.gov
    *         <path'> = /backbone/<host><path>
    */
  def toBackboneIRI(iri: omf#IRI)
  : omf#IRI

  /**
    * Produces the canonical "has...Source" IRI from the IRI of an entity relationship or data relationship to a structure
    *
    * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
    * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
    *         where:
    *         <fragment'> = has<fragment>Source
    */
  def toSourceIRI(iri: omf#IRI)
  : omf#IRI

  /**
    * Produces the canonical "has...Target" IRI for the IRI of an entity relationship or data relationship to a structure
    *
    * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
    * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
    *         where:
    *         <fragment'> = has<fragment>Target
    */
  def toTargetIRI(iri: omf#IRI)
  : omf#IRI


}

trait OMFStoreOps[omf <: OMF] { self : IRIOps[omf] =>

  implicit val itbTag: ClassTag[omf#ImmutableTerminologyBox]
  implicit val itgTag: ClassTag[omf#ImmutableTerminologyGraph]
  implicit val bTag: ClassTag[omf#ImmutableBundle]
  implicit val dTag: ClassTag[omf#ImmutableDescriptionBox]

  def getElementUUID
  (e: omf#Element)
  : UUID

  def getModuleIRI
  (m: omf#Module)
  : omf#IRI

  def getModuleName
  (m: omf#Module)
  : LocalName

  def getModuleUUID
  (m: omf#Module)
  : UUID

  def annotationProperties
  (m: omf#Module)
  (implicit store: omf#Store)
  : Seq[AnnotationProperty]

  def annotations
  (m: omf#Module)
  (implicit store: omf#Store)
  : Set[AnnotationPropertyValue]

  def foldModule[T]
  (funImmutableTerminologyGraph: omf#ImmutableTerminologyGraph => T,
   funMutableTerminologyGraph: omf#MutableTerminologyGraph => T,
   funImmutableTerminologyBundle: omf#ImmutableBundle => T,
   funMutableTerminologyBundle: omf#MutableBundle => T,
   funImmutableDescriptionBox: omf#ImmutableDescriptionBox => T,
   funMutableDescriptionBox: omf#MutableDescriptionBox => T)
  (m: omf#Module)
  : T

  def foldImmutableModule[T]
  (funImmutableTerminologyGraph: omf#ImmutableTerminologyGraph => T,
   funImmutableTerminologyBundle: omf#ImmutableBundle => T,
   funImmutableDescriptionBox: omf#ImmutableDescriptionBox => T)
  (t: omf#ImmutableModule)
  : T

  def foldMutableModule[T]
  (funMutableTerminologyGraph: omf#MutableTerminologyGraph => T,
   funMutableTerminologyBundle: omf#MutableBundle => T,
   funMutableDescriptionBox: omf#MutableDescriptionBox => T)
  (t: omf#MutableModule)
  : T

  final def foldTerminologyBox
  (t: omf#Module)
  : Option[omf#TerminologyBox]
  = foldModule[Option[omf#TerminologyBox]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b),
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldMutableTerminologyBox
  (t: omf#Module)
  : Option[omf#MutableTerminologyBox]
  = foldModule[Option[omf#MutableTerminologyBox]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b),
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldImmutableTerminologyBox
  (t: omf#Module)
  : Option[omf#ImmutableTerminologyBox]
  = foldModule[Option[omf#ImmutableTerminologyBox]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  def immutableTerminologyGraphSignature
  (ig: omf#ImmutableTerminologyGraph)
  : ImmutableTerminologyBoxSignature[omf]

  def mutableTerminologyGraphSignature
  (mg: omf#MutableTerminologyGraph)
  : MutableTerminologyBoxSignature[omf]

  final def foldTerminologyGraph
  (t: omf#Module)
  : Option[omf#TerminologyGraph]
  = foldModule[Option[omf#TerminologyGraph]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldMutableTerminologyGraph
  (t: omf#Module)
  : Option[omf#MutableTerminologyGraph]
  = foldModule[Option[omf#MutableTerminologyGraph]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldImmutableTerminologyGraph
  (t: omf#Module)
  : Option[omf#ImmutableTerminologyGraph]
  = foldModule[Option[omf#ImmutableTerminologyGraph]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  def immutableBundleSignature
  (ib: omf#ImmutableBundle)
  : ImmutableTerminologyBoxSignature[omf]

  def mutableBundleSignature
  (mb: omf#MutableBundle)
  : MutableTerminologyBoxSignature[omf]

  final def foldBundle
  (t: omf#Module)
  : Option[omf#Bundle]
  = foldModule[Option[omf#Bundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b),
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldMutableBundle
  (t: omf#Module)
  : Option[omf#MutableBundle]
  = foldModule[Option[omf#MutableBundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b),
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  final def foldImmutableBundle
  (t: omf#Module)
  : Option[omf#ImmutableBundle]
  = foldModule[Option[omf#ImmutableBundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  def immutableDescriptionBoxSignature
  (id: omf#ImmutableDescriptionBox)
  : ImmutableDescriptionBoxSignature[omf]

  def mutableDescriptionBoxSignature
  (md: omf#MutableDescriptionBox)
  : MutableDescriptionBoxSignature[omf]

  final def foldDescriptionBox
  (t: omf#Module)
  : Option[omf#DescriptionBox]
  = foldModule[Option[omf#DescriptionBox]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (d: omf#ImmutableDescriptionBox) => Some(d),
    funMutableDescriptionBox = (d: omf#MutableDescriptionBox) => Some(d)
  )(t)

  final def foldMutableDescriptionBox
  (t: omf#Module)
  : Option[omf#MutableDescriptionBox]
  = foldModule[Option[omf#MutableDescriptionBox]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (_: omf#ImmutableDescriptionBox) => None,
    funMutableDescriptionBox = (d: omf#MutableDescriptionBox) => Some(d)
  )(t)

  final def foldImmutableDescriptionBox
  (t: omf#Module)
  : Option[omf#ImmutableDescriptionBox]
  = foldModule[Option[omf#ImmutableDescriptionBox]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None,
    funImmutableDescriptionBox = (d: omf#ImmutableDescriptionBox) => Some(d),
    funMutableDescriptionBox = (_: omf#MutableDescriptionBox) => None
  )(t)

  /**
    * If supported, load the built-in datatype maps corresponding to OWL, RDFS, XML Schema 1.1 as a terminology graph
    *
    * @param store OMF storage provider
    * @return The DataRangeCategories corresponding to the OWL2-DL datatype map.
    */
  def loadBuiltinDatatypeMap
  ()
  (implicit store: omf#Store)
  : Throwables \/ DataRangeCategories[omf]

  def loadModule
  (m2i: Mutable2ImmutableModuleTable[omf],
   iri: omf#IRI)
  (implicit store: omf#Store)
  : Throwables \/ (omf#ImmutableModule, Mutable2ImmutableModuleTable[omf])

  final def loadTerminology
  (m2i: Mutable2ImmutableModuleTable[omf],
   iri: omf#IRI)
  (implicit store: omf#Store)
  : Throwables \/ (omf#ImmutableTerminologyBox, Mutable2ImmutableModuleTable[omf])
  = loadModule(m2i, iri).flatMap {
    case (tbox: omf#ImmutableTerminologyBox, table) =>
      (tbox -> table).right
    case (_: omf#ImmutableDescriptionBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"loadTerminology($iri) results in a DescriptionBox, not a TerminologyBox")).left
  }

  final def loadDescription
  (m2i: Mutable2ImmutableModuleTable[omf],
   iri: omf#IRI)
  (implicit store: omf#Store)
  : Throwables \/ (omf#ImmutableDescriptionBox, Mutable2ImmutableModuleTable[omf])
  = loadModule(m2i, iri).flatMap {
    case (dbox: omf#ImmutableDescriptionBox, table) =>
      (dbox -> table).right
    case (_: omf#ImmutableTerminologyBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"loadDescription($iri) results in a TerminologyBox, not a DescriptionBox")).left
  }

  def isMutable
  (m: omf#Module)
  (implicit store: omf#Store)
  : Boolean

  /*
   * Note: callers include createBuiltInDatatypeMaps
   */
  def asImmutableModule
  (m: omf#MutableModule,
   m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store)
  : Throwables \/
    (omf#ImmutableModule, Mutable2ImmutableModuleTable[omf])

  final def asImmutableTerminologyBox
  (m: omf#MutableModule,
   m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store)
  : Throwables \/
    (omf#ImmutableTerminologyBox, Mutable2ImmutableModuleTable[omf])
  = asImmutableModule(m, m2i).flatMap {
    case (tbox: omf#ImmutableTerminologyBox, table) =>
      (tbox -> table).right
    case (_: omf#ImmutableDescriptionBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableTerminology($m) results in a DescriptionBox, not a TerminologyBox")).left
  }

  final def asImmutableBundle
  (m: omf#MutableModule,
   m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store)
  : Throwables \/
    (omf#ImmutableBundle, Mutable2ImmutableModuleTable[omf])
  = asImmutableModule(m, m2i).flatMap {
    case (bundle: omf#ImmutableBundle, table) =>
      (bundle -> table).right
    case (tbox: omf#ImmutableTerminologyGraph, table) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableBundle($m) results in a TerminologyGraph, not a Bundle")).left
    case (_: omf#ImmutableDescriptionBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableBundle($m) results in a DescriptionBox, not a Bundle")).left
  }

  final def asImmutableTerminologyGraph
  (m: omf#MutableModule,
   m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store)
  : Throwables \/
    (omf#ImmutableTerminologyGraph, Mutable2ImmutableModuleTable[omf])
  = asImmutableModule(m, m2i).flatMap {
    case (tbox: omf#ImmutableTerminologyGraph, table) =>
      (tbox -> table).right
    case (tbox: omf#ImmutableBundle, table) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableTerminologyGraph($m) results in a Bundle, not a TerminologyGraph")).left
    case (_: omf#ImmutableDescriptionBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableTerminologyGraph($m) results in a DescriptionBox, not a TerminologyGraph")).left
  }

  final def asImmutableDescription
  (m: omf#MutableModule,
   m2i: Mutable2ImmutableModuleTable[omf])
  (implicit store: omf#Store)
  : Throwables \/
    (omf#ImmutableDescriptionBox, Mutable2ImmutableModuleTable[omf])
  = asImmutableModule(m, m2i).flatMap {
    case (dbox: omf#ImmutableDescriptionBox, table) =>
      (dbox -> table).right
    case (_: omf#ImmutableTerminologyBox, _) =>
      Set[java.lang.Throwable](
        OMFError.omfError(
          s"asImmutableDescription($m) results in a TerminologyBox, not a DescriptionBox")).left
  }

  def toMutableModule
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#MutableModule]

  def toTerminologyBox
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#TerminologyBox]

  def toImmutableTerminologyBox
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#ImmutableTerminologyBox]

  def toTerminologyGraph
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#TerminologyGraph]

  def toImmutableTerminologyGraph
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#ImmutableTerminologyGraph]

  def toBundle
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#Bundle]

  def toImmutableBundle
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#ImmutableBundle]

  def toDescriptionBox
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#DescriptionBox]

  def toImmutableDescriptionBox
  (m: omf#Module)
  (implicit store: omf#Store)
  : Option[omf#ImmutableDescriptionBox]

  def fromImmutableTerminology
  (tbox: omf#ImmutableTerminologyBox)
  (implicit store: omf#Store)
  : ImmutableTerminologyBoxSignature[omf]

  def fromMutableTerminology
  (tbox: omf#MutableTerminologyBox)
  (implicit store: omf#Store)
  : MutableTerminologyBoxSignature[omf]

  def fromImmutableDescriptionBox
  (dbox: omf#ImmutableDescriptionBox)
  (implicit store: omf#Store)
  : ImmutableDescriptionBoxSignature[omf]

  def fromMutableDescriptionBox
  (dbox: omf#MutableDescriptionBox)
  (implicit store: omf#Store)
  : MutableDescriptionBoxSignature[omf]

  def getTerminologyAxiomUUID
  (ax: omf#TerminologyAxiom)
  (implicit store: omf#Store)
  : UUID

  def getExtensionAxioms
  (extendingChildG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Iterable[omf#TerminologyExtensionAxiom]

  /**
    * Create a mutable terminology graph partially identified by an IRI and a kind.
    *
    * The complete identity of a graph includes the IRI, kind and imported/extended graphs.
    * For a mutable terminology graph, imported/extended graphs must be specified
    * via `addTerminologyGraphExtension`
    *
    * @param name the name of the new graph
    * @param iri  the identity of the new mutable terminology graph
    * @param kind the kind of the new mutable terminology graph
    * @param store manager
    * @return A new mutable terminology graph, if successful
    */
  protected def makeTerminologyGraph
  (name: LocalName,
   iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableTerminologyGraph

  protected def makeBundle
  (name: LocalName,
   iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableBundle

  /**
    * Create a mutable terminology graph using its iri for the graph name (last segment) and UUID (version 5).
    *
    * The complete identity of a graph includes the IRI, kind and imported/extended graphs.
    * For a mutable terminology graph, imported/extended graphs must be specified
    * via `addTerminologyGraphExtension`
    *
    * @param iri  the identity of the new mutable terminology graph from which
    *             to extract the graph name (last segment) and to generate a version 5 UUID
    * @param kind the kind of the new mutable terminology graph
    * @param store manager
    * @return A new mutable terminology graph, if successful
    */
  def makeTerminologyGraph
  (iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableTerminologyGraph
  = for {
    name <- lastSegment(iri)
    g <- makeTerminologyGraph(name, iri, kind)
  } yield g

  def makeBundle
  (iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableBundle
  = for {
    name <- lastSegment(iri)
    g <- makeBundle(name, iri, kind)
  } yield g

  def saveTerminology
  (g: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ Unit

  def saveTerminology
  (g: omf#TerminologyBox,
   os: java.io.OutputStream)
  (implicit store: omf#Store)
  : Throwables \/ Unit

  def makeDescriptionBox
  (iri: omf#IRI,
   kind: DescriptionKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableDescriptionBox
  = for {
    name <- lastSegment(iri)
    g <- makeDescriptionBox(name, iri, kind)
  } yield g

  protected def makeDescriptionBox
  (name: LocalName,
   iri: omf#IRI,
   k: DescriptionKind)
  (implicit store: omf#Store)
  : Throwables \/ omf#MutableDescriptionBox

  def saveDescriptionBox
  (g: omf#DescriptionBox)
  (implicit store: omf#Store)
  : Throwables \/ Unit

  def saveDescriptionBox
  (g: omf#DescriptionBox, os: java.io.OutputStream)
  (implicit store: omf#Store)
  : Throwables \/ Unit

  def resolveIRIAsLocalFile
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Throwables \/ File
}

trait ImmutableTerminologyGraphOps[omf <: OMF] { self: OMFStoreOps[omf] with IRIOps[omf] =>

  def getAnnotations
  (tbox: omf#TerminologyBox)
  : Set[AnnotationPropertyValue]

  def getTerminologyKind
  (tbox: omf#TerminologyBox)
  : TerminologyKind

  def lookupTerm
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Term]

  def lookupEntity
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Entity]

  def lookupAspect
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Aspect]

  def lookupConcept
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Concept]

  def lookupReifiedRelationship
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ReifiedRelationship]

  def lookupUnreifiedRelationship
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#UnreifiedRelationship]

  def lookupDataRange
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#DataRange]

  final def getDataRange
  (tbox: omf#TerminologyBox, name: LocalName, recursively: Boolean = true)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#DataRange
  = for {
    iri <- withFragment(getModuleIRI(tbox), name)
    dr <- lookupDataRange(tbox, iri, recursively) match {
      case Some(_dr) =>
        _dr.right
      case None =>
        Set[java.lang.Throwable](OMFError.omfError(
          s"getDataRange(tbox=${getModuleIRI(tbox)}, name=$name, recursively=$recursively}: not found!"
        )).left
    }
  } yield dr

  def restrictedDataRangeOf
  (dr: omf#DataRange)
  (implicit store: omf#Store)
  : Option[omf#DataRange]

  def lookupStructure
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Structure]

  def lookupEntityScalarDataProperty
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#EntityScalarDataProperty]

  def lookupEntityStructuredDataProperty
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#EntityStructuredDataProperty]

  def lookupScalarDataProperty
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ScalarDataProperty]

  def lookupStructuredDataProperty
  (tbox: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#StructuredDataProperty]

  def getAxiomUUID
  (ax: omf#Axiom)
  : UUID

  def getAxioms
  (tbox: omf#TerminologyBox)
  : (omf#IRI, Iterable[omf#Axiom])

  def getTerms
  (tbox: omf#TerminologyBox)
  : (omf#IRI, Iterable[omf#Term])

  def foldTerm[T]
  (funAspect: omf#Aspect => T,
   funConcept: omf#Concept => T,
   funReifiedRelationship: omf#ReifiedRelationship => T,
   funUnreifiedRelationship: omf#UnreifiedRelationship => T,
   funScalar: omf#Scalar => T,
   funStructure: omf#Structure => T,
   funScalarOneOfRestriction: omf#ScalarOneOfRestriction => T,
   funBinaryScalarRestriction: omf#BinaryScalarRestriction => T,
   funIRIScalarRestriction: omf#IRIScalarRestriction => T,
   funPlainLiteralScalarRestriction: omf#PlainLiteralScalarRestriction => T,
   funStringScalarRestriction: omf#StringScalarRestriction => T,
   funSynonymScalarRestriction: omf#SynonymScalarRestriction => T,
   funTimeScalarRestriction: omf#TimeScalarRestriction => T,
   funEntityScalarDataProperty: omf#EntityScalarDataProperty => T,
   funEntityStructuredDataProperty: omf#EntityStructuredDataProperty => T,
   funScalarDataProperty: omf#ScalarDataProperty => T,
   funStructuredDataProperty: omf#StructuredDataProperty => T,
   funChainRule: omf#ChainRule => T)
  (t: omf#Term)
  : T

  def getTermIRI
  (term: omf#Term)
  : omf#IRI

  def getTermName
  (term: omf#Term)
  : LocalName

  def getTermUUID
  (term: omf#Term)
  : UUID

  def foldBundleStatement[T]
  (funAnonymousConceptTaxonomyAxiom: omf#AnonymousConceptTaxonomyAxiom => T,
   funRootConceptTaxonomyAxiom: omf#RootConceptTaxonomyAxiom => T,
   funSpecificDisjointConceptAxiom: omf#SpecificDisjointConceptAxiom => T)
  (s: omf#TerminologyBundleStatement)
  : T

  def getConceptTreeDisjunctionUUID
  (ctd: omf#ConceptTreeDisjunction)
  : UUID

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  def lookupNestingAxiomForNestedChildIfAny
  (nestedG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Option[omf#TerminologyNestingAxiom]

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC).
    */
  def lookupNestingAxiomsForNestingContext
  (nestingC: omf#Concept)
  (implicit store: omf#Store)
  : Set[omf#TerminologyNestingAxiom]

  // entity concept

  /**
    * @param c A concept
    * @return A tuple consisting of:
    *         - the IRI of the concept
    *         - if any, the IRI of the graph corresponding to the concept
    *         - a boolean flag indicating whether this is an abstract concept or not
    * @since 0.10.3
    */
  def fromConcept
  (c: omf#Concept)
  : ConceptSignature[omf]

  def equivalentEntityConcepts
  (c1: Iterable[omf#Concept], c2: Iterable[omf#Concept])
  : Boolean = {
    val iris1 = c1.map(fromConcept).toSet
    val iris2 = c2.map(fromConcept).toSet
    val d = iris1.diff(iris2)
    d.isEmpty
  }

  // entity relationship


  /**
    * @param r , a relationship
    * @return a tuple consisting of:
    *         - the IRI of the relationship
    *         - the IRI of the graph corresponding to the relationship, if any
    *         - the source entity of the relationship
    *         - the target entity of the relationship
    *         - the characteristics of the relationship
    *         - a flag indicating whether the relationship is abstract or not.
    */
  def fromReifiedRelationship
  (r: omf#ReifiedRelationship)
  : ReifiedRelationshipSignature[omf]

  def fromUnreifiedRelationship
  (r: omf#UnreifiedRelationship)
  : UnreifiedRelationshipSignature[omf]

  /**
    * Compares the relationships in terms of their sources, target & characteristics
    * Does not compare the graphs corresponding to each relationship, if any	.
    *
    * @since 0.10.3
    */
  def equivalentEntityReifiedRelationships
  (r1: Iterable[omf#ReifiedRelationship],
   r2: Iterable[omf#ReifiedRelationship])
  : Boolean = {
    val left = r1.map { r =>
      val s = fromReifiedRelationship(r)
      (s.iri,
        getTermIRI(s.source),
        getTermIRI(s.target),
        relationshipCharacteristicsSummary(s.characteristics))
    }
      .toSet
    val right = r2.map { r =>
      val s = fromReifiedRelationship(r)
      (s.iri,
        getTermIRI(s.source),
        getTermIRI(s.target),
        relationshipCharacteristicsSummary(s.characteristics))
    }
      .toSet
    val d = left.diff(right)
    d.isEmpty
  }

  def fromEntityScalarDataProperty
  (esc: omf#EntityScalarDataProperty)
  : EntityScalarDataPropertySignature[omf]

  def fromEntityStructuredDataProperty
  (est: omf#EntityStructuredDataProperty)
  : EntityStructuredDataPropertySignature[omf]

  def fromScalarDataProperty
  (esc: omf#ScalarDataProperty)
  : ScalarDataPropertySignature[omf]

  def fromStructuredDataProperty
  (est: omf#StructuredDataProperty)
  : StructuredDataPropertySignature[omf]

  def foldAxiom[T]
  (funAspectSpecializationAxiom
   : omf#AspectSpecializationAxiom => T,
   funConceptSpecializationAxiom
   : omf#ConceptSpecializationAxiom => T,
   funReifiedRelationshipSpecializationAxiom
   : omf#ReifiedRelationshipSpecializationAxiom => T,
   funEntityExistentialRestrictionAxiom
   : omf#EntityExistentialRestrictionAxiom => T,
   funEntityUniversalRestrictionAxiom
   : omf#EntityUniversalRestrictionAxiom => T,
   funEntityScalarDataPropertyExistentialRestrictionAxiom
   : omf#EntityScalarDataPropertyExistentialRestrictionAxiom => T,
   funEntityScalarDataPropertyParticularRestrictionAxiom
   : omf#EntityScalarDataPropertyParticularRestrictionAxiom => T,
   funEntityScalarDataPropertyUniversalRestrictionAxiom
   : omf#EntityScalarDataPropertyUniversalRestrictionAxiom => T,
   funScalarOneOfLiteralAxiom
   : omf#ScalarOneOfLiteralAxiom => T)
  (t: omf#Axiom)
  : T

  def foldTerminologyBoxAxiom[T]
  ( funConceptDesignationTerminologyAxiom
    : omf#ConceptDesignationTerminologyAxiom => T,
    funTerminologyGraphDirectExtensionAxiom
    : omf#TerminologyExtensionAxiom => T,
    funTerminologyGraphDirectNestingAxiom
    : omf#TerminologyNestingAxiom => T)
  (t: omf#TerminologyBoxAxiom)
  : T

  def foldTerminologyBundleAxiom[T]
  ( funBundledTerminologyAxiom
    : omf#BundledTerminologyAxiom => T)
  (t: omf#TerminologyBundleAxiom)
  : T

  def fromAspectSubClassAxiom
  (ax: omf#AspectSpecializationAxiom)
  : AspectSpecializationSignature[omf]

  def fromConceptSpecializationAxiom
  (ax: omf#ConceptSpecializationAxiom)
  : ConceptSpecializationSignature[omf]

  def fromReifiedRelationshipSpecializationAxiom
  (ax: omf#ReifiedRelationshipSpecializationAxiom)
  : ReifiedRelationshipSpecializationSignature[omf]

  def fromEntityRestrictionAxiom
  (ax: omf#EntityRestrictionAxiom)
  : EntityRestrictionSignature[omf]

  def fromEntityScalarDataPropertyExistentialRestrictionAxiom
  (ax: omf#EntityScalarDataPropertyExistentialRestrictionAxiom)
  : EntityScalarDataPropertyQuantifiedRestrictionSignature[omf]

  def fromEntityScalarDataPropertyParticularRestrictionAxiom
  (ax: omf#EntityScalarDataPropertyParticularRestrictionAxiom)
  : EntityScalarDataPropertyParticularRestrictionSignature[omf]

  def fromEntityScalarDataPropertyUniversalRestrictionAxiom
  (ax: omf#EntityScalarDataPropertyUniversalRestrictionAxiom)
  : EntityScalarDataPropertyQuantifiedRestrictionSignature[omf]

  def fromScalarOneOfLiteralAxiom
  (ax: omf#ScalarOneOfLiteralAxiom)
  : ScalarOneOfLiteralSignature[omf]

  def fromBinaryScalarRestriction
  (ax: omf#BinaryScalarRestriction)
  : BinaryScalarRestrictionSignature[omf]

  def fromIRIScalarRestriction
  (ax: omf#IRIScalarRestriction)
  : IRIScalarRestrictionSignature[omf]

  def fromNumericScalarRestriction
  (ax: omf#NumericScalarRestriction)
  : NumericScalarRestrictionSignature[omf]

  def fromPlainLiteralScalarRestriction
  (ax: omf#PlainLiteralScalarRestriction)
  : PlainLiteralScalarRestrictionSignature[omf]

  def fromScalarOneOfRestriction
  (ax: omf#ScalarOneOfRestriction)
  : ScalarOneOfRestrictionSignature[omf]

  def fromStringScalarRestriction
  (ax: omf#StringScalarRestriction)
  : StringScalarRestrictionSignature[omf]

  def fromSynonymScalarRestriction
  (ax: omf#SynonymScalarRestriction)
  : SynonymScalarRestrictionSignature[omf]

  def fromTimeScalarRestriction
  (ax: omf#TimeScalarRestriction)
  : TimeScalarRestrictionSignature[omf]

  def fromConceptDesignationTerminologyAxiom
  (ax: omf#ConceptDesignationTerminologyAxiom)
  : ConceptDesignationTerminologySignature[omf]

  def fromTerminologyExtensionAxiom
  (ax: omf#TerminologyExtensionAxiom)
  : TerminologyExtensionSignature[omf]

  def fromTerminologyNestingAxiom
  (ax: omf#TerminologyNestingAxiom)
  : TerminologyNestingSignature[omf]

  def fromBundledTerminologyAxiom
  (ax: omf#BundledTerminologyAxiom)
  : BundledTerminologySignature[omf]

  def fromAnonymousConceptTaxonomyAxiom
  (ax: omf#AnonymousConceptTaxonomyAxiom)
  : AnonymousConceptUnionSignature[omf]

  def fromRootConceptTaxonomyAxiom
  (ax: omf#RootConceptTaxonomyAxiom)
  : RootConceptTaxonomySignature[omf]

  def fromSpecificDisjointConceptAxiom
  (ax: omf#SpecificDisjointConceptAxiom)
  : SpecificDisjointConceptSignature[omf]

  def fromChainRule
  (ax: omf#ChainRule)
  : ChainRuleSignature[omf]

  def fromRuleBodySegment
  (ax: omf#RuleBodySegment)
  : RuleBodySegmentSignature[omf]

  def fromAspectPredicate
  (ax: omf#AspectPredicate)
  : AspectPredicateSignature[omf]

  def fromConceptPredicate
  (ax: omf#ConceptPredicate)
  : ConceptPredicateSignature[omf]

  def fromReifiedRelationshipPredicate
  (ax: omf#ReifiedRelationshipPredicate)
  : ReifiedRelationshipPredicateSignature[omf]

  def fromReifiedRelationshipPropertyPredicate
  (ax: omf#ReifiedRelationshipPropertyPredicate)
  : ReifiedRelationshipPropertyPredicateSignature[omf]

  def fromReifiedRelationshipInversePropertyPredicate
  (ax: omf#ReifiedRelationshipInversePropertyPredicate)
  : ReifiedRelationshipInversePropertyPredicateSignature[omf]

  def fromReifiedRelationshipSourcePropertyPredicate
  (ax: omf#ReifiedRelationshipSourcePropertyPredicate)
  : ReifiedRelationshipSourcePropertyPredicateSignature[omf]

  def fromReifiedRelationshipSourceInversePropertyPredicate
  (ax: omf#ReifiedRelationshipSourceInversePropertyPredicate)
  : ReifiedRelationshipSourceInversePropertyPredicateSignature[omf]

  def fromReifiedRelationshipTargetPropertyPredicate
  (ax: omf#ReifiedRelationshipTargetPropertyPredicate)
  : ReifiedRelationshipTargetPropertyPredicateSignature[omf]

  def fromReifiedRelationshipTargetInversePropertyPredicate
  (ax: omf#ReifiedRelationshipTargetInversePropertyPredicate)
  : ReifiedRelationshipTargetInversePropertyPredicateSignature[omf]

  def fromUnreifiedRelationshipPropertyPredicate
  (ax: omf#UnreifiedRelationshipPropertyPredicate)
  : UnreifiedRelationshipPropertyPredicateSignature[omf]

  def fromUnreifiedRelationshipInversePropertyPredicate
  (ax: omf#UnreifiedRelationshipInversePropertyPredicate)
  : UnreifiedRelationshipInversePropertyPredicateSignature[omf]

}

trait MutableTerminologyGraphOps[omf <: OMF]
  extends ImmutableTerminologyGraphOps[omf] {
  self: OMFStoreOps[omf] with IRIOps[omf] =>

  def addTerminologyAnnotationProperty
  (graph: omf#MutableTerminologyBox,
   ap: AnnotationProperty)
  (implicit store: omf#Store)
  : Throwables \/ AnnotationProperty

  def addTerminologyAnnotation
  (graph: omf#MutableTerminologyBox,
   subject: omf#Element,
   property: AnnotationProperty,
   value: String)
  (implicit store: omf#Store)
  : Throwables \/ AnnotationPropertyValue

  def removeTerminologyAnnotations
  (graph: omf#MutableTerminologyBox,
   subject: omf#Element,
   property: AnnotationProperty)
  (implicit store: omf#Store)
  : Throwables \/ Set[AnnotationPropertyValue]

  /**
    * Add to a terminology graph a new OMF Aspect.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Aspect
    *
    * @param graph
    * @param uuid
    * @param aspectName
    * @param store
    * @return
    */
  protected def addAspect
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   aspectName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Aspect

  /**
    * Add to a terminology graph a new OMF Aspect
    * with a version 5 UUID based on the `graph` IRI and `aspectName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Aspect
    *
    * @param graph      : a terminology graph
    * @param aspectName : the name of a new entity aspect
    */
  final def addAspect
  (graph: omf#MutableTerminologyBox,
   aspectName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Aspect
  = for {
    iri <- withFragment(getModuleIRI(graph), aspectName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> aspectName)
    ax <- addAspect(graph, uuid, iri, aspectName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF Concept.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Concept
    * @param graph
    * @param uuid
    * @param conceptName
    * @param store
    * @return
    */
  protected def addConcept
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   conceptName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Concept

  /**
    * Add to a terminology graph a new OMF Concept
    * with a version 5 UUID based on the `graph` IRI and `conceptName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Concept
    *
    * @param graph       : a terminology graph
    * @param conceptName : the name of a new entity concept
    */
  final def addConcept
  (graph: omf#MutableTerminologyBox,
   conceptName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Concept
  = for {
    iri <- withFragment(getModuleIRI(graph), conceptName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> conceptName)
    ax <- addConcept(graph, uuid, iri, conceptName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF ReifiedRelationship.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationship
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param characteristics
    * @param reifiedRelationshipName
    * @param unreifiedRelationshipName
    * @param unreifiedInverseRelationshipName
    * @param store
    * @return
    */
  protected def addReifiedRelationship
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName])
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationship

  /**
    * Add to a terminology graph a new OMF ReifiedRelationship
    * with a version 5 UUID based on the `graph` IRI and `reifiedRelationshipName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationship
    *
    * @param graph                            a terminology graph
    * @param source                           an existing entity definition that will be
    *                                         the source of the new entity relationship
    * @param target                           an existing entity definition that will be
    *                                         the target of the new entity relationship
    * @param characteristics                  the characteristics of the new entity relationship
    * @param reifiedRelationshipName          the name of the new entity relationship
    *                                         from the perspective of a reified concept-like entity
    * @param unreifiedRelationshipName        the name of the entity relationship from the perspective
    *                                         of a directed property from the source to the target
    * @param unreifiedInverseRelationshipName if applicable, the name of the entity relationship from
    *                                         the perspective of a directed inverse property
    *                                         from the target to the source
    */
  final def addReifiedRelationship
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName])
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationship
  = for {
    iri <- withFragment(getModuleIRI(graph), reifiedRelationshipName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> reifiedRelationshipName)
    ax <- addReifiedRelationship(
      graph, uuid, iri, source, target, characteristics,
      reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF UnreifiedRelationship.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationship
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param characteristics
    * @param unreifiedRelationshipName
    * @param store
    * @return
    */
  protected def addUnreifiedRelationship
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   unreifiedRelationshipName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationship

  /**
    * Add to a terminology graph a new OMF UnreifiedRelationship
    * with a version 5 UUID based on the `graph` IRI and `unreifiedRelationshipName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.UnreifiedRelationship
    *
    * @param graph                            a terminology graph
    * @param source                           an existing entity definition that will be
    *                                         the source of the new entity relationship
    * @param target                           an existing entity definition that will be
    *                                         the target of the new entity relationship
    * @param characteristics                  the characteristics of the new entity relationship
    * @param unreifiedRelationshipName        the name of the unreified relationship from the perspective
    *                                         of a directed property from the source to the target
    */
  final def addUnreifiedRelationship
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   unreifiedRelationshipName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationship
  = for {
    iri <- withFragment(getModuleIRI(graph), unreifiedRelationshipName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> unreifiedRelationshipName)
    ax <- addUnreifiedRelationship(graph, uuid, iri, source, target, characteristics, unreifiedRelationshipName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF Scalar datatype.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Scalar
    *
    * @param graph
    * @param uuid
    * @param scalarDataTypeName
    * @param store
    * @return
    */
  protected def addScalarDataType
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   scalarDataTypeName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Scalar

  /**
    * Add to a terminology graph a new OMF Scalar datatype
    * with a version 5 UUID based on the `graph` IRI and `scalarDataTypeName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Scalar
    *
    * @param graph
    * @param scalarDataTypeName
    * @param store
    * @return
    */
  final def addScalarDataType
  (graph: omf#MutableTerminologyBox,
   scalarDataTypeName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Scalar
  = for {
    iri <- withFragment(getModuleIRI(graph), scalarDataTypeName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> scalarDataTypeName)
    ax <- addScalarDataType(graph, uuid, iri, scalarDataTypeName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF Structure datatype.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Structure
    *
    * @param graph
    * @param uuid
    * @param structureDatatypeName
    * @param store
    * @return
    */
  protected def addStructuredDataType
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   structureDatatypeName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Structure

  /**
    * Add to a terminology graph a new OMF Structure datatype
    * with a version 5 UUID based on the `graph` IRI and `structureDatatypeName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Structure
    *
    * @param graph
    * @param structureDatatypeName
    * @param store
    * @return
    */
  final def addStructuredDataType
  (graph: omf#MutableTerminologyBox,
   structureDatatypeName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#Structure
  = for {
    iri <- withFragment(getModuleIRI(graph), structureDatatypeName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> structureDatatypeName)
    ax <- addStructuredDataType(graph, uuid, iri, structureDatatypeName)
  } yield ax

  protected def addScalarOneOfRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: omf#IRI,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarOneOfRestriction

  final def addScalarOneOfRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarOneOfRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addScalarOneOfRestriction(graph, dataTypeUUID, dataTypeIRI, dataTypeName, restrictedRange)
  } yield ax

  def scalarOneOfLiteralAxiomUUID
  (graph: omf#MutableTerminologyBox,
   scalarOneOfRestriction: omf#ScalarOneOfRestriction)
  : Throwables \/ UUID
  = generateUUID(
      "ScalarOneOfLiteralAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "axiom" -> getTermUUID(scalarOneOfRestriction).toString).right

  protected def addScalarOneOfLiteralAxiom
  (graph: omf#MutableTerminologyBox,
   axiomUUID: UUID,
   scalarOneOfRestriction: omf#ScalarOneOfRestriction,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarOneOfLiteralAxiom

  final def addScalarOneOfLiteralAxiom
  (graph: omf#MutableTerminologyBox,
   scalarOneOfRestriction: omf#ScalarOneOfRestriction,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarOneOfLiteralAxiom
  = for {
    axiomUUID <- scalarOneOfLiteralAxiomUUID(graph, scalarOneOfRestriction)
    ax <- addScalarOneOfLiteralAxiom(graph, axiomUUID, scalarOneOfRestriction, value, valueType)
  } yield ax

  protected def addBinaryScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: omf#IRI,
   dataTypeName: LocalName,
   length: Option[PositiveIntegerLiteral],
   minLength: Option[PositiveIntegerLiteral],
   maxLength: Option[PositiveIntegerLiteral],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#BinaryScalarRestriction

  final def addBinaryScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[PositiveIntegerLiteral]=None,
   minLength: Option[PositiveIntegerLiteral]=None,
   maxLength: Option[PositiveIntegerLiteral]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#BinaryScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addBinaryScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, restrictedRange)
  } yield ax

  protected def addIRIScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[PositiveIntegerLiteral],
   minLength: Option[PositiveIntegerLiteral],
   maxLength: Option[PositiveIntegerLiteral],
   pattern: Option[LiteralPattern],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#IRIScalarRestriction

  final def addIRIScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[PositiveIntegerLiteral]=None,
   minLength: Option[PositiveIntegerLiteral]=None,
   maxLength: Option[PositiveIntegerLiteral]=None,
   pattern: Option[LiteralPattern]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#IRIScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addIRIScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, restrictedRange)
  } yield ax

  protected def addNumericScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   minInclusive: Option[LiteralNumber],
   maxInclusive: Option[LiteralNumber],
   minExclusive: Option[LiteralNumber],
   maxExclusive: Option[LiteralNumber],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#NumericScalarRestriction

  final def addNumericScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   minInclusive: Option[LiteralNumber]=None,
   maxInclusive: Option[LiteralNumber]=None,
   minExclusive: Option[LiteralNumber]=None,
   maxExclusive: Option[LiteralNumber]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#NumericScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addNumericScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      minInclusive, maxInclusive, minExclusive, maxExclusive, restrictedRange)
  } yield ax

  protected def addPlainLiteralScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[PositiveIntegerLiteral],
   minLength: Option[PositiveIntegerLiteral],
   maxLength: Option[PositiveIntegerLiteral],
   pattern: Option[LiteralPattern],
   language: Option[LangRange],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#PlainLiteralScalarRestriction

  final def addPlainLiteralScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[PositiveIntegerLiteral]=None,
   minLength: Option[PositiveIntegerLiteral]=None,
   maxLength: Option[PositiveIntegerLiteral]=None,
   pattern: Option[LiteralPattern]=None,
   language: Option[LangRange]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#PlainLiteralScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addPlainLiteralScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, language, restrictedRange)
  } yield ax

  protected def addStringScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[PositiveIntegerLiteral],
   minLength: Option[PositiveIntegerLiteral],
   maxLength: Option[PositiveIntegerLiteral],
   pattern: Option[LiteralPattern],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#StringScalarRestriction

  final def addStringScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[PositiveIntegerLiteral]=None,
   minLength: Option[PositiveIntegerLiteral]=None,
   maxLength: Option[PositiveIntegerLiteral]=None,
   pattern: Option[LiteralPattern]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#StringScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addStringScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, restrictedRange)
  } yield ax

  protected def addSynonymScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#SynonymScalarRestriction

  final def addSynonymScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#SynonymScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addSynonymScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      restrictedRange)
  } yield ax

  protected def addTimeScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   minInclusive: Option[LiteralDateTime],
   maxInclusive: Option[LiteralDateTime],
   minExclusive: Option[LiteralDateTime],
   maxExclusive: Option[LiteralDateTime],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#TimeScalarRestriction

  final def addTimeScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   minInclusive: Option[LiteralDateTime]=None,
   maxInclusive: Option[LiteralDateTime]=None,
   minExclusive: Option[LiteralDateTime]=None,
   maxExclusive: Option[LiteralDateTime]=None)
  (implicit store: omf#Store)
  : Throwables \/ omf#TimeScalarRestriction
  = for {
    dataTypeIRI <- withFragment(getModuleIRI(graph), dataTypeName)
    dataTypeUUID = generateUUID(getModuleUUID(graph), "name" -> dataTypeName)
    ax <- addTimeScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      minInclusive, maxInclusive, minExclusive, maxExclusive, restrictedRange)
  } yield ax

  /**
    * Add to a terminology graph a new OMF EntityScalarDataProperty.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityScalarDataProperty
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param dataPropertyName
    * @param isIdentityCriteria
    * @param store
    * @return
    */
  protected def addEntityScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#DataRange,
   dataPropertyName: LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataProperty

  /**
    *
    * Add to a terminology graph a new OMF EntityScalarDataProperty
    * with a version 5 UUID based on the `graph` and `source` IRIs and `dataPropertyName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityScalarDataProperty
    *
    * @param graph
    * @param source
    * @param target
    * @param dataPropertyName
    * @param isIdentityCriteria
    * @param store
    * @return
    */
  final def addEntityScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#DataRange,
   dataPropertyName: LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataProperty
  = for {
    iri <- withFragment(getModuleIRI(graph),dataPropertyName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> dataPropertyName)
    ax <- addEntityScalarDataProperty(graph, uuid, iri, source, target, dataPropertyName, isIdentityCriteria)
  } yield ax

  /**
    * Add to a terminology graph a new OMF EntityStructuredDataProperty.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityStructuredDataProperty
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param dataPropertyName
    * @param isIdentityCriteria
    * @param store
    * @return
    */
  protected def addEntityStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#Structure,
   dataPropertyName: LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityStructuredDataProperty

  /**
    * Add to a terminology graph a new OMF EntityStructuredDataProperty
    * with a version 5 UUID based on the `graph` and `source` IRIs and `dataPropertyName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityStructuredDataProperty
    *
    * @param graph
    * @param source
    * @param target
    * @param dataPropertyName
    * @param isIdentityCriteria
    * @param store
    * @return
    */
  final def addEntityStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#Structure,
   dataPropertyName: LocalName,
   isIdentityCriteria: Boolean)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityStructuredDataProperty
  = for {
    iri <- withFragment(getModuleIRI(graph), dataPropertyName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> dataPropertyName)
    ax <- addEntityStructuredDataProperty(graph, uuid, iri, source, target, dataPropertyName, isIdentityCriteria)
  } yield ax

  /**
    * Add to a terminology graph an OMF ScalarDataProperty.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ScalarDataProperty
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param dataPropertyName
    * @param store
    * @return
    */
  protected def addScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Structure,
   target: omf#DataRange,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarDataProperty

  /**
    * Add to a terminology graph an OMF ScalarDataProperty
    * with a version 5 UUID based on the `graph` and `source` IRIs and `dataPropertyName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ScalarDataProperty
    *
    * @param graph
    * @param source
    * @param target
    * @param dataPropertyName
    * @param store
    * @return
    */
  final def addScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Structure,
   target: omf#DataRange,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarDataProperty
  = for {
    iri <- withFragment(getModuleIRI(graph), dataPropertyName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> dataPropertyName)
    ax <- addScalarDataProperty(graph, uuid, iri, source, target, dataPropertyName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF StructuredDataProperty.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.StructuredDataProperty
    *
    * @param graph
    * @param uuid
    * @param source
    * @param target
    * @param dataPropertyName
    * @param store
    * @return
    */
  protected def addStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Structure,
   target: omf#Structure,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#StructuredDataProperty

  /**
    * Add to a terminology graph a new OMF StructuredDataProperty
    * with a version 5 UUID based on the `graph` and `source` IRIs and `dataPropertyName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.StructuredDataProperty
    *
    * @param graph
    * @param source
    * @param target
    * @param dataPropertyName
    * @param store
    * @return
    */
  final def addStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Structure,
   target: omf#Structure,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#StructuredDataProperty
  = for {
    iri <- withFragment(getModuleIRI(graph), dataPropertyName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> dataPropertyName)
    ax <- addStructuredDataProperty(graph, uuid, iri, source, target, dataPropertyName)
  } yield ax

  protected def addChainRule
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   head: omf#UnreifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ChainRule

  final def addChainRule
  (graph: omf#MutableTerminologyBox,
   head: omf#UnreifiedRelationship,
   ruleName: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ChainRule
  = for {
    iri <- withFragment(getModuleIRI(graph), ruleName)
    uuid = generateUUID(getModuleUUID(graph), "name" -> ruleName)
    ax <- addChainRule(graph, uuid, iri, head)
  } yield ax

  protected def addRuleBodySegment
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   chainRule: Option[omf#ChainRule],
   previousSegment: Option[omf#RuleBodySegment])
  (implicit store: omf#Store)
  : Throwables \/ omf#RuleBodySegment

  final def addRuleBodySegment
  (graph: omf#MutableTerminologyBox,
   chainRule: Option[omf#ChainRule],
   previousSegment: Option[omf#RuleBodySegment])
  (implicit store: omf#Store)
  : Throwables \/ omf#RuleBodySegment
  = for {
      cr <- chainRule match {
        case Some(cr) =>
          fromChainRule(cr).uuid.toString.right
        case None =>
          previousSegment match {
            case Some(ps) =>
              fromRuleBodySegment(ps).position.toString.right
            case None =>
              Set[java.lang.Throwable](
                OMFError.omfError(
                  "addRuleBodySegment: either the chainRule or the previousSegment must be specified"
                )).left
          }
      }
      ps = previousSegment match {
        case Some(seg) =>
          fromRuleBodySegment(seg).position.toString
        case None =>
          "1"
      }
      uuid = generateUUID(
        "RuleBodySegment",
        "chainRule" -> cr,
        "position" -> ps)
      ax <- addRuleBodySegment(graph, uuid, chainRule, previousSegment)
  } yield ax

  protected def addAspectPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   aspect: omf#Aspect)
  (implicit store: omf#Store)
  : Throwables \/ omf#AspectPredicate

  final def addAspectPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   aspect: omf#Aspect)
  (implicit store: omf#Store)
  : Throwables \/ omf#AspectPredicate
  = {
    val termUUID = getTermUUID(aspect).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "AspectPredicate",
      "aspect" -> termUUID,
      "bodySegment" -> bodySegmentUUID)
    addAspectPredicate(graph, uuid, bodySegment, aspect)
  }

  protected def addConceptPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   concept: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptPredicate

  final def addConceptPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   concept: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptPredicate
  = {
    val termUUID = getTermUUID(concept).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ConceptPredicate",
      "bodySegment" -> bodySegmentUUID,
      "concept" -> termUUID)
    addConceptPredicate(graph, uuid, bodySegment, concept)
  }

  protected def addReifiedRelationshipPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipPredicate

  final def addReifiedRelationshipPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipPropertyPredicate

  final def addReifiedRelationshipPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipPropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipPropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipPropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInversePropertyPredicate

  final def addReifiedRelationshipInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInversePropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipInversePropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipInversePropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipSourcePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSourcePropertyPredicate

  final def addReifiedRelationshipSourcePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSourcePropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipSourcePropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipSourcePropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipSourceInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSourceInversePropertyPredicate

  final def addReifiedRelationshipSourceInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSourceInversePropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipSourceInversePropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipSourceInversePropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipTargetPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipTargetPropertyPredicate

  final def addReifiedRelationshipTargetPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipTargetPropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipTargetPropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipTargetPropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addReifiedRelationshipTargetInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipTargetInversePropertyPredicate

  final def addReifiedRelationshipTargetInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   reifiedRelationship: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipTargetInversePropertyPredicate
  = {
    val termUUID = getTermUUID(reifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "ReifiedRelationshipTargetInversePropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "reifiedRelationship" -> termUUID)
    addReifiedRelationshipTargetInversePropertyPredicate(graph, uuid, bodySegment, reifiedRelationship)
  }

  protected def addUnreifiedRelationshipPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   unreifiedRelationship: omf#UnreifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipPropertyPredicate

  final def addUnreifiedRelationshipPropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   unreifiedRelationship: omf#UnreifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipPropertyPredicate
  = {
    val termUUID = getTermUUID(unreifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "UnreifiedRelationshipPropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "unreifiedRelationship" -> termUUID)
    addUnreifiedRelationshipPropertyPredicate(graph, uuid, bodySegment, unreifiedRelationship)
  }

  protected def addUnreifiedRelationshipInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   bodySegment: omf#RuleBodySegment,
   unreifiedRelationship: omf#UnreifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipInversePropertyPredicate

  final def addUnreifiedRelationshipInversePropertyPredicate
  (graph: omf#MutableTerminologyBox,
   bodySegment: omf#RuleBodySegment,
   unreifiedRelationship: omf#UnreifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipInversePropertyPredicate
  = {
    val termUUID = getTermUUID(unreifiedRelationship).toString
    val bodySegmentUUID = fromRuleBodySegment(bodySegment).uuid.toString
    val uuid = generateUUID(
      "UnreifiedRelationshipInversePropertyPredicate",
      "bodySegment" -> bodySegmentUUID,
      "unreifiedRelationship" -> termUUID)
    addUnreifiedRelationshipInversePropertyPredicate(graph, uuid, bodySegment, unreifiedRelationship)
  }

  // model term axioms

  /**
    * Add to a terminology graph a new OMF AspectSpecializationAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.AspectSpecializationAxiom
    *
    * @param graph
    * @param uuid
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  protected def addAspectSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Throwables \/ omf#AspectSpecializationAxiom

  def aspectSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "AspectSpecializationAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "superAspect" -> getTermUUID(sup).toString,
      "subEntity" -> getTermUUID(sub).toString).right

  /**
    * Add to a terminology graph a new OMF AspectSpecializationAxiom
    * with a version 5 UUID based on the `graph`, `sub` and `sup` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.AspectSpecializationAxiom
    *
    * @param graph
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  final def addAspectSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Throwables \/ omf#AspectSpecializationAxiom
  = for {
    uuid <- aspectSpecializationAxiomUUID(graph, sub, sup)
    ax <- addAspectSpecializationAxiom(graph, uuid, sub, sup)
  } yield ax

  def conceptSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Concept,
   sup: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "ConceptSpecializationAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "superConcept" -> getTermUUID(sup).toString,
      "subConcept" -> getTermUUID(sub).toString).right

  /**
    * Add to a terminology graph a new OMF ConceptSpecializationAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptSpecializationAxiom
    *
    * @param graph
    * @param uuid
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  protected def addConceptSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Concept,
   sup: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptSpecializationAxiom

  /**
    * Add to a terminology graph a new OMF ConceptSpecializationAxiom
    * with a version 5 UUID based on the `graph`, `sub`, `sup` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptSpecializationAxiom
    *
    * @param graph
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  final def addConceptSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Concept,
   sup: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptSpecializationAxiom
  = for {
    uuid <- conceptSpecializationAxiomUUID(graph, sub, sup)
    ax <- addConceptSpecializationAxiom(graph, uuid, sub, sup)
  } yield ax

  def reifiedRelationshipSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#ReifiedRelationship,
   sup: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "ReifiedRelationshipSpecializationAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "superRelationship" -> getTermUUID(sup).toString,
      "subRelationship" -> getTermUUID(sub).toString).right

  /**
    * Add to a terminology graph a new OMF ReifiedRelationshipSpecializationAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationshipSpecializationAxiom
    *
    * @param graph
    * @param uuid
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  protected def addReifiedRelationshipSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#ReifiedRelationship,
   sup: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSpecializationAxiom

  /**
    * Add to a terminology graph a new OMF ReifiedRelationshipSpecializationAxiom
    * with a version 5 UUID based on the `graph`, `sub`, `sup` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationshipSpecializationAxiom
    *
    * @param graph
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  final def addReifiedRelationshipSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#ReifiedRelationship,
   sup: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipSpecializationAxiom
  = for {
    uuid <- reifiedRelationshipSpecializationAxiomUUID(graph, sub, sup)
    ax <- addReifiedRelationshipSpecializationAxiom(graph, uuid, sub, sup)
  } yield ax

  /**
    * Add to a terminology graph a new OMF EntityUniversalRestrictionAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityUniversalRestrictionAxiom
    *
    * @param graph
    * @param uuid
    * @param sub
    * @param rel
    * @param range
    * @param store
    * @return
    */
  protected def addEntityUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityUniversalRestrictionAxiom

  def entityUniversalRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "EntityUniversalRestrictionAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "restrictedRelation" -> getTermUUID(rel).toString,
      "restrictedDomain" -> getTermUUID(sub).toString,
      "restrictedRange" -> getTermUUID(range).toString).right

  /**
    * Add to a terminology graph a new OMF EntityUniversalRestrictionAxiom
    * with a version 5 UUID based on the `graph`, `sub`, 'rel` and 'range` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityUniversalRestrictionAxiom
    *
    * @param graph
    * @param sub
    * @param rel
    * @param range
    * @param store
    * @return
    */
  final def addEntityUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(graph, sub, rel, range)
    ax <- addEntityUniversalRestrictionAxiom(graph, uuid, sub, rel, range)
  } yield ax

  /**
    * Add to a terminology graph a new OMF EntityExistentialRestrictionAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityExistentialRestrictionAxiom
    *
    * @param graph
    * @param uuid
    * @param sub
    * @param rel
    * @param range
    * @param store
    * @return
    */
  protected def addEntityExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityExistentialRestrictionAxiom

  def entityExistentialRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "EntityExistentialRestrictionAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "restrictedRelation" -> getTermUUID(rel).toString,
      "restrictedDomain" -> getTermUUID(sub).toString,
      "restrictedRange" -> getTermUUID(range).toString).right

  /**
    * Add to a terminology graph a new OMF EntityExistentialRestrictionAxiom
    * with a version 5 UUID based on the `graph`, `sub`, `rel` and `range` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityExistentialRestrictionAxiom
    *
    * @param graph
    * @param sub
    * @param rel
    * @param range
    * @param store
    * @return
    */
  final def addEntityExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#EntityRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(graph, sub, rel, range)
    ax <- addEntityExistentialRestrictionAxiom(graph, uuid, sub, rel, range)
  } yield ax

  protected def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyExistentialRestrictionAxiom

  def entityScalarDataPropertyExistentialRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "EntityScalarDataPropertyExistentialRestrictionAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "restrictedEntity" -> getTermUUID(restrictedEntity).toString,
      "scalarProperty" -> getTermUUID(scalarProperty).toString,
      "scalarRestriction" -> getTermUUID(range).toString).right

  final def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyExistentialRestrictionAxiom
  = for {
    uuid <- entityScalarDataPropertyExistentialRestrictionAxiomUUID(graph, restrictedEntity, scalarProperty, range)
    ax <- addEntityScalarDataPropertyExistentialRestrictionAxiom(graph, uuid, restrictedEntity, scalarProperty, range)
  } yield ax

  protected def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyUniversalRestrictionAxiom

  def entityScalarDataPropertyUniversalRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "EntityScalarDataPropertyUniversalRestrictionAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "restrictedEntity" -> getTermUUID(restrictedEntity).toString,
      "scalarProperty" -> getTermUUID(scalarProperty).toString,
      "scalarRestriction" -> getTermUUID(range).toString).right

  final def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyUniversalRestrictionAxiom
  = for {
    uuid <- entityScalarDataPropertyUniversalRestrictionAxiomUUID(graph, restrictedEntity, scalarProperty, range)
    ax <- addEntityScalarDataPropertyUniversalRestrictionAxiom(graph, uuid, restrictedEntity, scalarProperty, range)
  } yield ax

  protected def addEntityScalarDataPropertyParticularRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   literalValue: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyParticularRestrictionAxiom

  def entityScalarDataPropertyParticularRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "EntityScalarDataPropertyParticularRestrictionAxiom",
      "tbox" -> getModuleUUID(graph).toString,
      "restrictedEntity" -> getTermUUID(restrictedEntity).toString,
      "scalarProperty" -> getTermUUID(scalarProperty).toString).right

  final def addEntityScalarDataPropertyParticularRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   literalValue: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#EntityScalarDataPropertyParticularRestrictionAxiom
  = for {
    uuid <- entityScalarDataPropertyParticularRestrictionAxiomUUID(graph, restrictedEntity, scalarProperty)
    ax <- addEntityScalarDataPropertyParticularRestrictionAxiom(graph, uuid, restrictedEntity, scalarProperty, literalValue, valueType)
  } yield ax

  /**
    * Add to a terminology graph a new OMF TerminologyExtensionAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyExtensionAxiom
    *
    * @param uuid
    * @param extendingTerminology
    * @param extendedTerminology
    * @param store
    * @return
    */
  protected def addTerminologyExtension
  (uuid: UUID,
   extendingTerminology: omf#MutableTerminologyBox,
   extendedTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#TerminologyExtensionAxiom

  def terminologyExtensionUUID
  (extendingG: omf#MutableTerminologyBox,
   extendedG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "TerminologyExtensionAxiom",
      "tbox" -> getModuleUUID(extendingG).toString,
      "extendedTerminology" -> getModuleUUID(extendedG).toString).right

  /**
    * Add to a terminology graph a new OMF TerminologyExtensionAxiom
    * with a version 5 UUID based on the `extendingG` and `extendedG` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyExtensionAxiom
    *
    * @param extendingG
    * @param extendedG
    * @param store
    * @return
    */
  final def addTerminologyExtension
  (extendingG: omf#MutableTerminologyBox,
   extendedG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#TerminologyExtensionAxiom
  = for {
    uuid <- terminologyExtensionUUID(extendingG, extendedG)
    ax <- addTerminologyExtension(uuid, extendingG, extendedG)
  } yield ax

  /**
    * Add to a terminology graph a new OMF TerminologyNestingAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyNestingAxiom
    *
    * @param uuid
    * @param nestingTerminology
    * @param nestingContext
    * @param nestedTerminology
    * @param store
    * @return
    */
  protected def addNestedTerminology
  (uuid: UUID,
   nestingTerminology: omf#TerminologyBox,
   nestingContext: omf#Concept,
   nestedTerminology: omf#MutableTerminologyGraph)
  (implicit store: omf#Store)
  : Throwables \/ omf#TerminologyNestingAxiom

  def terminologyNestingAxiomUUID
  (nestingGraph: omf#TerminologyBox,
   nestingContext: omf#Concept,
   nestedGraph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "TerminologyNestingAxiom",
      "tbox" -> getModuleUUID(nestedGraph).toString,
      "nestingTerminology" -> getModuleUUID(nestingGraph).toString,
      "nestingContext" -> getTermUUID(nestingContext).toString).right

  /**
    * Add to a terminology graph a new OMF TerminologyNestingAxiom
    * with a version 5 UUID based on the `nestingContext` and `nestedGraph` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyNestingAxiom
    *
    * @param nestingGraph
    * @param nestingContext
    * @param nestedGraph
    * @param store
    * @return
    */
  final def addNestedTerminology
  (nestingGraph: omf#TerminologyBox,
   nestingContext: omf#Concept,
   nestedGraph: omf#MutableTerminologyGraph)
  (implicit store: omf#Store)
  : Throwables \/ omf#TerminologyNestingAxiom
  = for {
    uuid <- terminologyNestingAxiomUUID(nestingGraph, nestingContext, nestedGraph)
    ax <- addNestedTerminology(uuid, nestingGraph, nestingContext, nestedGraph)
  } yield ax

  /**
    * Add to a terminology graph a new OMF ConceptDesignationTerminologyAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptDesignationTerminologyAxiom
    *
    * @param graph                 The mutable terminology graph in which to assert the axiom
    * @param designatedConcept     The concept whose complete complete designation is specified
    * @param designatedTerminology The terminology graph specifying the complete designation
    *                              for the structural contents of the designated concept
    * @param store                 OMF storage provider
    * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
    */
  protected def addEntityConceptDesignationTerminologyAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   designatedConcept: omf#Concept,
   designatedTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptDesignationTerminologyAxiom

  /**
    * Add to a terminology graph a new OMF ConceptDesignationTerminologyAxiom
    * with a version 5 UUID based on `graph`, `entityConceptDesignation` and `designationTerminologyGraph` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptDesignationTerminologyAxiom
    *
    * @param graph                       The mutable terminology graph in which to assert the axiom
    * @param entityConceptDesignation    The model entity concept whose complete designation is specified
    * @param designationTerminologyGraph The terminology graph specifying the complete designation
    *                                    for the structural contents of the model entity concept
    * @param store                       OMF storage provider
    * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
    */
  final def addEntityConceptDesignationTerminologyAxiom
  (graph: omf#MutableTerminologyBox,
   entityConceptDesignation: omf#Concept,
   designationTerminologyGraph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptDesignationTerminologyAxiom
  = for {
    ax <- addEntityConceptDesignationTerminologyAxiom(
      graph,
      generateUUID(
        "ConceptDesignationTerminologyAxiom",
        "tbox" -> getModuleUUID(graph).toString,
        "designatedConcept" -> getTermUUID(entityConceptDesignation).toString,
        "designatedTerminology" -> getModuleUUID(designationTerminologyGraph).toString),
      entityConceptDesignation,
      designationTerminologyGraph)
  } yield ax

  /**
    * Add to a terminology graph a new OMF BundledTerminologyAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.BundledTerminologyAxiom
    *
    * @param uuid
    * @param terminologyBundle
    * @param bundledTerminology
    * @param store
    * @return
    */
  protected def addBundledTerminologyAxiom
  (uuid: UUID,
   terminologyBundle: omf#MutableBundle,
   bundledTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#BundledTerminologyAxiom

  def bundledTerminologyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   bundledTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "BundledTerminologyAxiom",
    "bundledTerminology" -> getModuleUUID(bundledTerminology).toString,
    "bundle" -> getModuleUUID(terminologyBundle).toString).right

  /**
    * Add to a terminology graph a new OMF BundledTerminologyAxiom
    * with a version 5 UUID based on the `bundlingG` and `bundledG` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.BundledTerminologyAxiom
    *
    * @param terminologyBundle
    * @param bundledTerminology
    * @param store
    * @return
    */
  final def addBundledTerminologyAxiom
  (terminologyBundle: omf#MutableBundle,
   bundledTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#BundledTerminologyAxiom
  = for {
    uuid <- bundledTerminologyAxiomUUID(terminologyBundle, bundledTerminology)
    ax <- addBundledTerminologyAxiom(uuid, terminologyBundle, bundledTerminology)
  } yield ax

  //

  /**
    * Add to a terminology graph a new OMF RootConceptTaxonomyAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.RootConceptTaxonomyAxiom
    *
    * @param uuid
    * @param terminologyBundle
    * @param root
    * @param store
    * @return
    */
  protected def addRootConceptTaxonomyAxiom
  (uuid: UUID,
   terminologyBundle: omf#MutableBundle,
   root: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#RootConceptTaxonomyAxiom

  def rootConceptTaxonomyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   root: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "RootConceptTaxonomyAxiom",
    "bundle" -> getModuleUUID(terminologyBundle).toString,
    "root" -> getTermUUID(root).toString).right

  /**
    * Add to a terminology graph a new OMF RootConceptTaxonomyAxiom
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.RootConceptTaxonomyAxiom
    *
    * @param terminologyBundle
    * @param root
    * @param store
    * @return
    */
  final def addRootConceptTaxonomyAxiom
  (terminologyBundle: omf#MutableBundle,
   root: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#RootConceptTaxonomyAxiom
  = for {
    uuid <- rootConceptTaxonomyAxiomUUID(terminologyBundle, root)
    ax <- addRootConceptTaxonomyAxiom(uuid, terminologyBundle, root)
  } yield ax

  //

  /**
    * Add to a terminology graph a new OMF AnonymousConceptTaxonomyAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.AnonymousConceptTaxonomyAxiom
    *
    * @param uuid
    * @param terminologyBundle
    * @param disjointTerminologyParent
    * @param store
    * @return
    */
  protected def addAnonymousConceptTaxonomyAxiom
  (uuid: UUID,
   terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   name: String)
  (implicit store: omf#Store)
  : Throwables \/ omf#AnonymousConceptTaxonomyAxiom

  def anonymousConceptTaxonomyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   name: String)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    getElementUUID(disjointTerminologyParent).toString,
    "name" -> name).right

  /**
    * Add to a terminology graph a new OMF AnonymousConceptTaxonomyAxiom
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.AnonymousConceptTaxonomyAxiom
    *
    * @param terminologyBundle
    * @param disjointTerminologyParent
    * @param store
    * @return
    */
  final def addAnonymousConceptTaxonomyAxiom
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   name: String)
  (implicit store: omf#Store)
  : Throwables \/ omf#AnonymousConceptTaxonomyAxiom
  = for {
    uuid <- anonymousConceptTaxonomyAxiomUUID(terminologyBundle, disjointTerminologyParent, name)
    ax <- addAnonymousConceptTaxonomyAxiom(uuid, terminologyBundle, disjointTerminologyParent, name)
  } yield ax

  //

  /**
    * Add to a terminology graph a new OMF SpecificDisjointConceptAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.SpecificDisjointConceptAxiom
    *
    * @param uuid
    * @param terminologyBundle
    * @param disjointTerminologyParent
    * @param disjointLeaf
    * @param store
    * @return
    */
  protected def addSpecificDisjointConceptAxiom
  (uuid: UUID,
   terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   disjointLeaf: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#SpecificDisjointConceptAxiom

  def specificDisjointConceptAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   disjointLeaf: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
      "SpecificDisjointConceptAxiom",
      "disjointTaxonomyParent" -> getElementUUID(disjointTerminologyParent).toString,
      "disjointLeaf" -> getTermUUID(disjointLeaf).toString).right

  /**
    * Add to a terminology graph a new OMF SpecificDisjointConceptAxiom
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.SpecificDisjointConceptAxiom
    *
    * @param terminologyBundle
    * @param disjointTerminologyParent
    * @param disjointLeaf
    * @param store
    * @return
    */
  final def addSpecificDisjointConceptAxiom
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   disjointLeaf: omf#Concept)
  (implicit store: omf#Store)
  : Throwables \/ omf#SpecificDisjointConceptAxiom
  = for {
    uuid <- specificDisjointConceptAxiomUUID(terminologyBundle, disjointTerminologyParent, disjointLeaf)
    ax <- addSpecificDisjointConceptAxiom(uuid, terminologyBundle, disjointTerminologyParent, disjointLeaf)
  } yield ax

}

trait ImmutableDescriptionBoxOps[omf <: OMF] { self: OMFStoreOps[omf] with IRIOps[omf] =>

  def getImmutableDescriptionBoxIRI
  (graph: omf#ImmutableDescriptionBox)
  : omf#IRI

  def fromConceptInstance
  (o: omf#ConceptInstance)
  : ConceptInstanceSignature[omf]

  def fromReifiedRelationshipInstance
  (r: omf#ReifiedRelationshipInstance)
  : ReifiedRelationshipInstanceSignature[omf]

  def fromReifiedRelationshipInstanceDomain
  (r: omf#ReifiedRelationshipInstanceDomain)
  : ReifiedRelationshipInstanceDomainSignature[omf]

  def fromReifiedRelationshipInstanceRange
  (r: omf#ReifiedRelationshipInstanceRange)
  : ReifiedRelationshipInstanceRangeSignature[omf]

  def fromUnreifiedRelationshipInstanceTuple
  (ur: omf#UnreifiedRelationshipInstanceTuple)
  : UnreifiedRelationshipInstanceTupleSignature[omf]

  def fromSingletonInstanceScalarDataPropertyValue
  (e2sc: omf#SingletonInstanceScalarDataPropertyValue)
  : SingletonInstanceScalarDataPropertyValueSignature[omf]

  def fromSingletonInstanceStructuredDataPropertyValue
  (dbox: omf#DescriptionBox, e2sc: omf#SingletonInstanceStructuredDataPropertyValue)
  : SingletonInstanceStructuredDataPropertyValueSignature[omf]

  def fromScalarDataPropertyValue
  (e2sc: omf#ScalarDataPropertyValue)
  : ScalarDataPropertyValueSignature[omf]

  def fromStructuredDataPropertyTuple
  (dbox: omf#DescriptionBox, e2sc: omf#StructuredDataPropertyTuple)
  : StructuredDataPropertyTupleSignature[omf]

  def fromDescriptionBoxRefinementAxiom
  (ax: omf#DescriptionBoxRefinement)
  : DescriptionBoxRefinementSignature[omf]

  def fromClosedWorldDefinitionsAxiom
  (ax: omf#DescriptionBoxExtendsClosedWorldDefinitions)
  : DescriptionBoxExtendsClosedWorldDefinitionsSignature[omf]

}

trait MutableDescriptionBoxOps[omf <: OMF]
  extends ImmutableDescriptionBoxOps[omf] {
  self: OMFStoreOps[omf] with IRIOps[omf] =>

  def addDescriptionAnnotationProperty
  (dbox: omf#MutableDescriptionBox,
   ap: AnnotationProperty)
  (implicit store: omf#Store)
  : Throwables \/ AnnotationProperty

  def addDescriptionAnnotation
  (dbox: omf#MutableDescriptionBox,
   subject: omf#Element,
   property: AnnotationProperty,
   value: String)
  (implicit store: omf#Store)
  : Throwables \/ AnnotationPropertyValue

  def removeDescriptionAnnotations
  (dbox: omf#MutableDescriptionBox,
   subject: omf#Element,
   property: AnnotationProperty)
  (implicit store: omf#Store)
  : Throwables \/ Set[AnnotationPropertyValue]

  def getMutableDescriptionBoxIRI
  (dbox: omf#MutableDescriptionBox)
  : omf#IRI

  def descriptionBoxExtendsClosedWorldDefinitionsUUID
  (dbox: omf#MutableDescriptionBox,
   closedWorldDefinitions: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "DescriptionBoxExtendsClosedWorldDefinitions",
    "descriptionBox" -> getModuleUUID(dbox).toString,
    "closedWorldDefinitions" -> getModuleUUID(closedWorldDefinitions).toString).right

  protected def addDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   closedWorldDefinitions: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#DescriptionBoxExtendsClosedWorldDefinitions

  def addDescriptionBoxExtendsClosedWorldDefinitions
  (dbox: omf#MutableDescriptionBox,
   closedWorldDefinitions: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#DescriptionBoxExtendsClosedWorldDefinitions
  = for {
    uuid <- descriptionBoxExtendsClosedWorldDefinitionsUUID(dbox, closedWorldDefinitions)
    ax <- addDescriptionBoxExtendsClosedWorldDefinitions(uuid, dbox, closedWorldDefinitions)
  } yield ax

  def descriptionBoxRefinementUUID
  (refiningDescriptionBox: omf#MutableDescriptionBox,
   refinedDescriptionBox: omf#DescriptionBox)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "DescriptionBoxRefinement",
    "refiningDescriptionBox" -> getModuleUUID(refiningDescriptionBox).toString,
    "refinedDescriptionBox" -> getModuleUUID(refinedDescriptionBox).toString).right

  protected def addDescriptionBoxRefinement
  (uuid: UUID,
   refiningDescriptionBox: omf#MutableDescriptionBox,
   refinedDescriptionBox: omf#DescriptionBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#DescriptionBoxRefinement

  def addDescriptionBoxRefinement
  (refiningDescriptionBox: omf#MutableDescriptionBox,
   refinedDescriptionBox: omf#DescriptionBox)
  (implicit store: omf#Store)
  : Throwables \/ omf#DescriptionBoxRefinement
  = for {
    uuid <- descriptionBoxRefinementUUID(refiningDescriptionBox, refinedDescriptionBox)
    ax <- addDescriptionBoxRefinement(uuid, refiningDescriptionBox, refinedDescriptionBox)
  } yield ax

  def conceptInstanceUUID
  (dbox: omf#MutableDescriptionBox,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(getModuleUUID(dbox).toString, "name" -> fragment).right

  protected def addConceptInstance
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   iri: omf#IRI,
   conceptType: omf#Concept,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptInstance

  def addConceptInstance
  (dbox: omf#MutableDescriptionBox,
   conceptType: omf#Concept,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ConceptInstance
  = for {
    iri <- withFragment(getModuleIRI(dbox), fragment)
    uuid <- conceptInstanceUUID(dbox, fragment)
    ax <- addConceptInstance(uuid, dbox, iri, conceptType, fragment)
  } yield ax

  def reifiedRelationshipInstanceUUID
  (dbox: omf#MutableDescriptionBox,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(getModuleUUID(dbox).toString, "name" -> fragment).right

  protected def addReifiedRelationshipInstance
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   iri: omf#IRI,
   relationshipType: omf#ReifiedRelationship,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstance

  def addReifiedRelationshipInstance
  (dbox: omf#MutableDescriptionBox,
   relationshipType: omf#ReifiedRelationship,
   fragment: LocalName)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstance
  = for {
    iri <- withFragment(getModuleIRI(dbox), fragment)
    uuid <- reifiedRelationshipInstanceUUID(dbox, fragment)
    ax <- addReifiedRelationshipInstance(uuid, dbox, iri, relationshipType, fragment)
  } yield ax

  def reifiedRelationshipInstanceDomainUUID
  (dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   source: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "ReifiedRelationshipInstanceDomain",
    "descriptionBox" -> getModuleUUID(dbox).toString,
    "reifiedRelationshipInstance" -> getElementUUID(relationshipInstance).toString,
    "domain" -> getElementUUID(source).toString).right

  protected def addReifiedRelationshipInstanceDomain
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   source: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstanceDomain

  def addReifiedRelationshipInstanceDomain
  (dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   source: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstanceDomain
  = for {
    uuid <- reifiedRelationshipInstanceDomainUUID(dbox, relationshipInstance, source)
    ax <- addReifiedRelationshipInstanceDomain(uuid, dbox, relationshipInstance, source)
  } yield ax

  def reifiedRelationshipInstanceRangeUUID
  (dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "ReifiedRelationshipInstanceRange",
    "descriptionBox" -> getModuleUUID(dbox).toString,
    "reifiedRelationshipInstance" -> getElementUUID(relationshipInstance).toString,
    "range" -> getElementUUID(target).toString).right

  protected def addReifiedRelationshipInstanceRange
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstanceRange

  def addReifiedRelationshipInstanceRange
  (dbox: omf#MutableDescriptionBox,
   relationshipInstance: omf#ReifiedRelationshipInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#ReifiedRelationshipInstanceRange
  = for {
    uuid <- reifiedRelationshipInstanceRangeUUID(dbox, relationshipInstance, target)
    ax <- addReifiedRelationshipInstanceRange(uuid, dbox, relationshipInstance, target)
  } yield ax

  def unreifiedRelationshipInstanceTupleUUID
  (dbox: omf#MutableDescriptionBox,
   unreifiedRelationship: omf#UnreifiedRelationship,
   source: omf#ConceptualEntitySingletonInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "UnreifiedRelationshipInstanceTuple",
    "descriptionBox" -> getModuleUUID(dbox).toString,
    "unreifiedRelationship" -> getElementUUID(unreifiedRelationship).toString,
    "romain" -> getElementUUID(source).toString,
    "range" -> getElementUUID(target).toString).right

  protected def addUnreifiedRelationshipInstanceTuple
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   unreifiedRelationship: omf#UnreifiedRelationship,
   source: omf#ConceptualEntitySingletonInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipInstanceTuple

  def addUnreifiedRelationshipInstanceTuple
  (dbox: omf#MutableDescriptionBox,
   unreifiedRelationship: omf#UnreifiedRelationship,
   source: omf#ConceptualEntitySingletonInstance,
   target: omf#ConceptualEntitySingletonInstance)
  (implicit store: omf#Store)
  : Throwables \/ omf#UnreifiedRelationshipInstanceTuple
  = for {
    uuid <- unreifiedRelationshipInstanceTupleUUID(dbox, unreifiedRelationship, source, target)
    ax <- addUnreifiedRelationshipInstanceTuple(uuid, dbox, unreifiedRelationship, source, target)
  } yield ax

  def singletonInstanceScalarDataPropertyValueUUID
  (dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2sc: omf#EntityScalarDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID().right

  protected def addSingletonInstanceScalarDataPropertyValue
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2sc: omf#EntityScalarDataProperty,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#SingletonInstanceScalarDataPropertyValue

  def addSingletonInstanceScalarDataPropertyValue
  (dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2sc: omf#EntityScalarDataProperty,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#SingletonInstanceScalarDataPropertyValue
  = for {
    uuid <- singletonInstanceScalarDataPropertyValueUUID(dbox, ei, e2sc)
    ax <- addSingletonInstanceScalarDataPropertyValue(uuid, dbox, ei, e2sc, value, valueType)
  } yield ax

  def singletonInstanceStructuredDataPropertyValueUUID
  (dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2st: omf#EntityStructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "SingletonInstanceStructuredDataPropertyValue",
    "descriptionBox" -> getModuleUUID(dbox).toString,
    "singletonInstance" -> getElementUUID(ei).toString,
    "structuredDataProperty" -> getElementUUID(e2st).toString
  ).right

  protected def addSingletonInstanceStructuredDataPropertyValue
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2st: omf#EntityStructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ omf#SingletonInstanceStructuredDataPropertyValue

  def addSingletonInstanceStructuredDataPropertyValue
  (dbox: omf#MutableDescriptionBox,
   ei: omf#ConceptualEntitySingletonInstance,
   e2st: omf#EntityStructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ omf#SingletonInstanceStructuredDataPropertyValue
  = for {
    uuid <- singletonInstanceStructuredDataPropertyValueUUID(dbox, ei, e2st)
    ax <- addSingletonInstanceStructuredDataPropertyValue(uuid, dbox, ei, e2st)
  } yield ax

  def scalarDataPropertyValueUUID
  (structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   scalarDataProperty: omf#ScalarDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "ScalarDataPropertyValue",
    "scalarDataProperty" -> getElementUUID(scalarDataProperty).toString,
    "structuredDataPropertyContext" -> getElementUUID(structuredDataPropertyContext).toString
  ).right

  protected def makeScalarDataPropertyValue
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   scalarDataProperty: omf#ScalarDataProperty,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarDataPropertyValue

  def makeScalarDataPropertyValue
  (dbox: omf#MutableDescriptionBox,
   structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   scalarDataProperty: omf#ScalarDataProperty,
   value: LiteralValue,
   valueType: Option[omf#DataRange])
  (implicit store: omf#Store)
  : Throwables \/ omf#ScalarDataPropertyValue
  = for {
    uuid <- scalarDataPropertyValueUUID(structuredDataPropertyContext, scalarDataProperty)
    ax <- makeScalarDataPropertyValue(uuid, dbox, structuredDataPropertyContext,
      scalarDataProperty, value, valueType)
  } yield ax

  def structuredDataPropertyTupleUUID
  (structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   structuredDataProperty: omf#StructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ UUID
  = generateUUID(
    "StructuredDataPropertyTuple",
    "structuredDataProperty" -> getElementUUID(structuredDataProperty).toString,
    "structuredDataPropertyContext" -> getElementUUID(structuredDataPropertyContext).toString
  ).right

  protected def makeStructuredDataPropertyTuple
  (uuid: UUID,
   dbox: omf#MutableDescriptionBox,
   structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   structuredDataProperty: omf#StructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ omf#StructuredDataPropertyTuple

  def makeStructuredDataPropertyTuple
  (dbox: omf#MutableDescriptionBox,
   structuredDataPropertyContext: omf#SingletonInstanceStructuredDataPropertyContext,
   structuredDataProperty: omf#StructuredDataProperty)
  (implicit store: omf#Store)
  : Throwables \/ omf#StructuredDataPropertyTuple
  = for {
    uuid <- structuredDataPropertyTupleUUID(structuredDataPropertyContext, structuredDataProperty)
    ax <- makeStructuredDataPropertyTuple(uuid, dbox, structuredDataPropertyContext, structuredDataProperty)
  } yield ax

}

trait OMFOps[omf <: OMF]
  extends IRIOps[omf]
    with MutableTerminologyGraphOps[omf]
    with MutableDescriptionBoxOps[omf]
    with OMFStoreOps[omf]