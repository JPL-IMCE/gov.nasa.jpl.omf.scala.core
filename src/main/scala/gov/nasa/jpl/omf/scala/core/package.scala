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

package gov.nasa.jpl.omf.scala

import java.util.UUID

import com.fasterxml.uuid.Generators
import com.fasterxml.uuid.impl.NameBasedGenerator
import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty}

import scala.{Boolean,Int,Ordering}
import scala.collection.immutable._
import scala.Predef.String

package object core {

  implicit def annotationPropertyOrdering
  : Ordering[AnnotationProperty]
  = new Ordering[AnnotationProperty] {

    def compare(x: AnnotationProperty, y: AnnotationProperty)
    : Int
    = x.uuid.compareTo(y.uuid)

  }

  implicit def annotationOrdering
  : Ordering[AnnotationEntry]
  = new Ordering[AnnotationEntry] {

    def compare(x: AnnotationEntry, y: AnnotationEntry)
    : Int
    = x.moduleUUID.compareTo(y.moduleUUID) match {
      case c if 0 != c =>
        c
      case 0 =>
        x.subjectUUID.compareTo(y.subjectUUID) match {
          case c if 0 != c =>
            c
          case 0 =>
            x.value.compareTo(y.value)
        }
    }

  }

  /**
    * Version 4 random UUID
    * @see https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_.28random.29
    *
    * @return Version 4 random UUID
    */
  def generateUUID()
  : UUID
  = UUID.randomUUID()

  /**
    * Version 5 UUID based on a URL name in the standard URL namespace.
    *
    * @param url The url to encode as a version 5 UUID in the standard URL namespace.
    * @return Version 5 UUID encoding of the url
    */
  def generateUUID(url: String)
  : UUID
  = Generators
    .nameBasedGenerator(NameBasedGenerator.NAMESPACE_URL)
    .generate(url)

  def getImportedTerminologies[Omf <: OMF]
  (tbox: Omf#TerminologyBox,
   onlyCompatibleKind: Boolean = true  )
  ( implicit ops: OMFOps[Omf], store: Omf#Store )
  : Set[Omf#TerminologyBox]
  = {

    val s = ops.fromTerminology(tbox)

    def hasCompatibleKind(g: Omf#TerminologyBox)
    : Boolean
    = !onlyCompatibleKind || TerminologyKind.compatibleKind(s.kind)(ops.getTerminologyKind(g))

    val imported = s.imports.filter(hasCompatibleKind).to[Set]
    imported
  }

  /**
   * The reflexive transitive closure of terminology graphs reachable by following
   * TerminologyGraphDirectImportAxiom (from importing to imported) and
   * TerminologyGraphDirectNestingParentAxiom (from nested child to nesting parent).
   *
   * @param g The terminology graph whose direct & indirect imports and nesting parents are included in the result
   *          (subject to kind filtering)
   * @param onlyCompatibleKind determines the filtering for imported & nesting parent terminology graphs
   * @return gs:
   *         if onlyCompatibleKind is true; then gs contains g and all directly or indirectly
   *         imported / nesting parents g' where g has compatible kind with g'
   *         if onlyCompatibleKind is false; then gs contains g and all directly or indirectly
   *         imported / nesting parents g' regardless of whether g is compatible with g'
   *
   * If:
   * TerminologyGraphDirectImportAxiom(importing=G1, imported=G2)
   * TerminologyGraphDirectImportAxiom(importing=G2, imported=G3)
   * Then:
   * G1 imports G2,G3
   * G2 imports G3
   *
   * If:
   * TerminologyGraphDirectImportAxiom(importing=G1, imported=G2)
   * TerminologyGraphDirectNestingParentAxiom(nestedChild=G2, nestingParent=G3)
   * TerminologyGraphDirectImportAxiom(importing=G3, imported=G4)
   * Then:
   * G1 imports G2,G3,G4
   * G3 imports G4
   *
   * If:
   * TerminologyGraphDirectImportAxiom(importing=G1, imported=G2a)
   * TerminologyGraphDirectNestingParentAxiom(nestedChild=G2a, nestingParent=G3)
   * TerminologyGraphDirectNestingParentAxiom(nestedChild=G2b, nestingParent=G3)
   * TerminologyGraphDirectImportAxiom(importing=G3, imported=G4)
   * Then:
   * G1 imports G2a,G3,G4
   * G3 imports G4
   */
  def terminologyImportClosure[Omf <: OMF, TG <: Omf#TerminologyBox]
  ( g: TG,
    onlyCompatibleKind: Boolean = true )
  ( implicit ops: OMFOps[Omf], store: Omf#Store )
  : Set[Omf#TerminologyBox] = {

    def step
    (gi: Omf#TerminologyBox)
    : Set[Omf#TerminologyBox]
    = getImportedTerminologies(gi, onlyCompatibleKind)

    val result
    : Set[Omf#TerminologyBox]
    = OMFOps.closure[Omf#TerminologyBox, Omf#TerminologyBox](g, step) + g

    result
  }

  /**
   * Aggregates all entities defined in terminology graphs
   * @param tboxes: a set of terminology graphs
   * @return a 3-tuple of the aspects, concepts and relationships entities defined in the graphs:
   */
  def allEntities[Omf <: OMF]
  ( tboxes: Set[Omf#TerminologyBox] )
  ( implicit ops: OMFOps[Omf], store: Omf#Store )
  : (
    Set[Omf#Aspect],
    Set[Omf#Concept],
    Set[Omf#ReifiedRelationship],
    Set[Omf#UnreifiedRelationship]) = {

    import ops._

    val entities0 =
      ( Set[Omf#Aspect](), Set[Omf#Concept](), Set[Omf#ReifiedRelationship](), Set[Omf#UnreifiedRelationship]() )
    val entitiesN =
      ( entities0 /: ( for { tbox <- tboxes } yield {
        val s =
          fromTerminology( tbox )
        ( s.aspects.toSet, s.concepts.toSet, s.reifiedRelationships.toSet, s.unreifiedRelationships.toSet )
      } ) ) { case ( ( ai, ci, ri, ui ), ( aj, cj, rj, uj) ) => ( ai ++ aj, ci ++ cj, ri ++ rj, ui ++ uj ) }

    entitiesN
  }

}