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

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.{Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.String
import scala.collection.immutable.{Iterable, Set}
import scalaz._

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
  : Set[java.lang.Throwable] \/ omf#IRI

  def getFragment(iri: omf#IRI)
  : Set[java.lang.Throwable] \/ String

  def withFragment(iri: omf#IRI, fragment: String)
  : Set[java.lang.Throwable] \/ omf#IRI

  /**
    * Split the IRI in two components: the IRI without the fragment, the IRI fragment
    */
  def splitIRI(iri: omf#IRI)
  : (omf#IRI, Option[String])

  /**
    * If the IRI has a fragment, returns "n:f" where "n" is the last segment of the IRI and "f" is the fragment of the IRI
    */
  def toAbbreviatedName(iri: omf#IRI, lowercaseFragmentInitial: Boolean)
  : Option[String]

  /**
    * Extract the last segment of a fragment-less IRI
    *
    * @param iri An IRI without a fragment
    * @return The last segment of the IRI (i.e., the name after the last '/')
    */
  def lastSegment(iri: omf#IRI)
  : Set[java.lang.Throwable] \/ LocalName

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

  def lookupTerminologyGraph
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Option[omf#ModelTerminologyGraph]

  def lookupTerminologyGraph
  (uuid: UUID)
  (implicit store: omf#Store)
  : Option[omf#ModelTerminologyGraph]

  /**
    * If supported, load the built-in datatype maps corresponding to OWL, RDFS, XML Schema 1.1 as a terminology graph
    *
    * @param store OMF storage provider
    * @return If supported, the terminology graph with the built-in datatype maps corresponding to
    *         the datatype maps defined in OWL, RDFS, XML Schema 1.1 and the map of
    *         mutable/immutable terminology graphs conversions that have been performed.
    */
  def loadBuiltinDatatypeMap
  ()
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableModelTerminologyGraph, omf#Mutable2ImmutableTerminologyMap)


  def loadTerminologyGraph
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ (omf#ImmutableModelTerminologyGraph, omf#Mutable2ImmutableTerminologyMap)

  def isTerminologyGraphMutable
  (graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean

  def asMutableTerminologyGraph
  (graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Option[omf#MutableModelTerminologyGraph]

  def isTerminologyGraphImmutable
  (graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean

  def asImmutableTerminologyGraph
  (graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Option[omf#ImmutableModelTerminologyGraph]

  def fromTerminologyGraph
  (graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : TerminologyGraphSignature[omf]

  def getTerminologyGraphAxiomUUID
  (ax: omf#TerminologyGraphAxiom)
  (implicit store: omf#Store)
  : UUID

  def isTerminologyGraphDirectNestingAxiom
  (axiom: omf#TerminologyGraphAxiom)
  (implicit store: omf#Store)
  : Boolean

  /**
    * Find the axiom TerminologyGraphDirectNestingAxiom(nestedChild==nestedG), if any.
    */
  def lookupNestingAxiomForNestedChildIfAny
  (nestedG: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Option[omf#TerminologyGraphDirectNestingAxiom]

  /**
    * Find the axioms TerminologyGraphDirectNestingAxiom(nestingContext=nestingC).
    */
  def lookupNestingAxiomsForNestingContext
  (nestingC: omf#ModelEntityConcept)
  (implicit store: omf#Store)
  : Set[omf#TerminologyGraphDirectNestingAxiom]

  def getNestingGraphOfAxiom
  (axiom: omf#TerminologyGraphDirectNestingAxiom)
  (implicit store: omf#Store)
  : omf#ModelTerminologyGraph

  def getNestingContextConceptOfAxiom
  (axiom: omf#TerminologyGraphDirectNestingAxiom)
  (implicit store: omf#Store)
  : omf#ModelEntityConcept

  def getExtendedGraphOfTerminologogyGraphDirectExtensionAxiom
  (gax: omf#TerminologyGraphDirectExtensionAxiom)
  (implicit store: omf#Store)
  : omf#ModelTerminologyGraph

  def getDirectlyExtendedGraphsOfExtendingChildGraph
  (extendingChildG: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Iterable[omf#TerminologyGraphDirectExtensionAxiom]

  /**
    * Create a mutable terminology graph partially identified by an IRI and a kind.
    *
    * The complete identity of a graph includes the IRI, kind and imported/extended graphs.
    * For a mutable terminology graph, imported/extended graphs must be specified
    * via `addTerminologyGraphExtension`
    *
    * @param uuid A version 4 (random) or 5 (name) based UUID.
    *             For version 5, use generateUUID(fromIRI(iri))
    * @param name the name of the new graph
    * @param iri  the identity of the new mutable terminology graph
    * @param kind the kind of the new mutable terminology graph
    * @param store manager
    * @return A new mutable terminology graph, if successful
    */
  protected def makeTerminologyGraph
  (uuid: UUID,
   name: LocalName,
   iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#MutableModelTerminologyGraph

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
  : Set[java.lang.Throwable] \/ omf#MutableModelTerminologyGraph
  = for {
    name <- lastSegment(iri)
    g <- makeTerminologyGraph(generateUUID(fromIRI(iri)+"#TerminologyGraph"), name, iri, kind)
  } yield g

  def saveTerminologyGraph
  (g: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ Unit

  def saveTerminologyGraph
  (g: omf#ModelTerminologyGraph,
   os: java.io.OutputStream)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ Unit

  /**
    * Converts a mutable tbox graph into an equivalent immutable tbox graph such that
    * dependencies on mutable tbox graphs are also converted into equivalent immutable tbox graphs.
    *
    * @param g a mutable tbox
    * @param store
    * @return a map of all the mutable tboxes (incl. g) that have been converted to immutable tboxes
    */
  def asImmutableTerminologyGraph
  (g: omf#MutableModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableModelTerminologyGraph, omf#Mutable2ImmutableTerminologyMap)

  def asImmutableTerminologyGraph
  (m2i: omf#Mutable2ImmutableTerminologyMap,
   g: omf#MutableModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableModelTerminologyGraph, omf#Mutable2ImmutableTerminologyMap)

  def isEntityDefinitionAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    (s.aspects.toSet contains t) ||
      (s.concepts.toSet contains t) || (
      s.reifiedRelationships.toSet contains t)
  }

  def isEntityDefinitionImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityDefinitionAssertedInTerminologyGraph(t, _))

  def isEntityUnreifiedRelationshipAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.reifiedRelationships.toSet contains t
  }

  def isEntityUnreifiedRelationshipImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityUnreifiedRelationshipAssertedInTerminologyGraph(t, _))

  def isScalarDataTypeAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.scalarDataTypes.toSet contains t
  }

  def isScalarDataTypeImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isScalarDataTypeAssertedInTerminologyGraph(t, _))

  def isStructuredDataTypeAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.structuredDataTypes.toSet contains t
  }

  def isStructuredDataTypeImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isStructuredDataTypeAssertedInTerminologyGraph(t, _))

  def isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.entity2scalarDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromEntityToScalarImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph(t, _))

  def isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.entity2structureDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromEntityToStructureImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph(t, _))

  def isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.structure2scalarDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromStructureToScalarImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph(t, _))

  def isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Boolean = {
    val s = fromTerminologyGraph(graph)
    s.structure2structureDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromStructureToStructureImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph(t, _))

  def isTypeTermAssertedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    isEntityDefinitionAssertedInTerminologyGraph(t, graph) ||
      isEntityUnreifiedRelationshipAssertedInTerminologyGraph(t, graph) ||
      isScalarDataTypeAssertedInTerminologyGraph(t, graph) ||
      isStructuredDataTypeAssertedInTerminologyGraph(t, graph) ||
      isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph(t, graph) ||
      isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph(t, graph) ||
      isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph(t, graph) ||
      isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph(t, graph)

  def isTypeTermImportedInTerminologyGraph
  (t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists(isTypeTermAssertedInTerminologyGraph(t, _))

  def loadInstanceGraph
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ImmutableModelInstanceGraph

  def fromInstanceGraph
  (graph: omf#ModelInstanceGraph)
  : (omf#IRI,
    Iterable[omf#ImmutableModelTerminologyGraph],
    Iterable[omf#ModelInstanceGraph],
    Iterable[omf#ModelInstanceObject],
    Iterable[omf#ModelInstanceRelation],
    Iterable[omf#ModelInstanceDataLiteral],
    Iterable[omf#ModelInstanceDataStructure],
    Iterable[omf#ModelInstanceDataRelationshipFromEntityToScalar],
    Iterable[omf#ModelInstanceDataRelationshipFromEntityToStructure],
    Iterable[omf#ModelInstanceDataRelationshipFromStructureToScalar],
    Iterable[omf#ModelInstanceDataRelationshipFromStructureToStructure])

  def makeInstanceGraph
  (iri: omf#IRI,
   instantiatedTGraphs: Iterable[omf#ImmutableModelTerminologyGraph],
   extendedIGraphs: Iterable[omf#ImmutableModelInstanceGraph])
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#MutableModelInstanceGraph

  def asImmutableInstanceGraph
  (g: omf#MutableModelInstanceGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ImmutableModelInstanceGraph

  def saveInstanceGraph
  (g: omf#ModelInstanceGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ Unit

  /**
    * @since 0.10.2
    */
  def saveInstanceGraph
  (g: omf#ModelInstanceGraph, os: java.io.OutputStream)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ Unit

}

trait ImmutableTerminologyGraphOps[omf <: OMF] { self: OMFStoreOps[omf] with IRIOps[omf] =>

  def getTerminologyGraphIRI
  (graph: omf#ModelTerminologyGraph)
  : omf#IRI

  def getTerminologyGraphLocalName
  (graph: omf#ModelTerminologyGraph)
  : LocalName

  def getTerminologyGraphUUID
  (graph: omf#ModelTerminologyGraph)
  : UUID

  def getTerminologyGraphKind
  (graph: omf#ModelTerminologyGraph)
  : TerminologyKind

  def lookupTypeTerm
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelTypeTerm]

  def lookupEntityDefinition
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelEntityDefinition]

  def lookupEntityDefinitionScalarDataRelationshipRestrictions
  (graph: omf#ModelTerminologyGraph, entity: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]
  = ImmutableTerminologyGraphOps
    .lookupEntityDefinitionScalarDataRelationshipRestrictions(this, graph, entity)

  def lookupEntityAspect
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelEntityAspect]

  def lookupEntityConcept
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelEntityConcept]

  def lookupEntityReifiedRelationship
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelEntityReifiedRelationship]

  def lookupEntityUnreifiedRelationship
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelEntityUnreifiedRelationship]

  def lookupScalarDataType
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelScalarDataType]

  def lookupStructuredDataType
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelStructuredDataType]

  def lookupEntityDataRelationshipFromEntityToScalar
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelDataRelationshipFromEntityToScalar]

  def lookupEntityDataRelationshipFromEntityToStructure
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelDataRelationshipFromEntityToStructure]

  def lookupEntityDataRelationshipFromStructureToScalar
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelDataRelationshipFromStructureToScalar]

  def lookupEntityDataRelationshipFromStructureToStructure
  (graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ModelDataRelationshipFromStructureToStructure]

  def getTermAxiomUUID
  (ax: omf#ModelTermAxiom)
  : UUID

  def getTermAxioms
  (graph: omf#ModelTerminologyGraph)
  : (omf#IRI, Iterable[omf#ModelTermAxiom])

  def getTypeTerms
  (graph: omf#ModelTerminologyGraph)
  : (omf#IRI, Iterable[omf#ModelTypeTerm])

  def foldTerm[T]
  (funEntityAspect: omf#ModelEntityAspect => T,
   funEntityConcept: omf#ModelEntityConcept => T,
   funEntityReifiedRelationship: omf#ModelEntityReifiedRelationship => T,
   funEntityUnreifiedRelationship: omf#ModelEntityUnreifiedRelationship => T,
   funScalarDataType: omf#ModelScalarDataType => T,
   funStructuredDataType: omf#ModelStructuredDataType => T,
   funDataRelationshipFromEntityToScalar: omf#ModelDataRelationshipFromEntityToScalar => T,
   funDataRelationshipFromEntityToStructure: omf#ModelDataRelationshipFromEntityToStructure => T,
   funDataRelationshipFromStructureToScalar: omf#ModelDataRelationshipFromStructureToScalar => T,
   funDataRelationshipFromStructureToStructure: omf#ModelDataRelationshipFromStructureToStructure => T)
  (t: omf#ModelTypeTerm)
  : T

  def getTermLocalName
  (term: omf#ModelTypeTerm)
  : LocalName

  def getTermUUID
  (term: omf#ModelTypeTerm)
  : UUID

  def fromTerm
  (t: omf#ModelTypeTerm)
  : omf#IRI =
    foldTerm[omf#IRI](
      (ea: omf#ModelEntityAspect) =>
        fromEntityAspect(ea),
      (ec: omf#ModelEntityConcept) =>
        fromEntityConcept(ec).iri,
      (er: omf#ModelEntityReifiedRelationship) =>
        fromEntityReifiedRelationship(er).iri,
      (ur: omf#ModelEntityUnreifiedRelationship) =>
        fromEntityUnreifiedRelationship(ur).iri,
      (sc: omf#ModelScalarDataType) =>
        fromScalarDataType(sc)._3,
      (sd: omf#ModelStructuredDataType) =>
        fromStructuredDataType(sd)._3,
      (esc: omf#ModelDataRelationshipFromEntityToScalar) =>
        fromDataRelationshipFromEntityToScalar(esc)._3,
      (est: omf#ModelDataRelationshipFromEntityToStructure) =>
        fromDataRelationshipFromEntityToStructure(est)._3,
      (ssc: omf#ModelDataRelationshipFromStructureToScalar) =>
        fromDataRelationshipFromStructureToScalar(ssc)._3,
      (sst: omf#ModelDataRelationshipFromStructureToStructure) =>
        fromDataRelationshipFromStructureToStructure(sst)._3)(t)

  // entity aspect

  def fromEntityAspect(t: omf#ModelEntityAspect)
  : omf#IRI

  // entity definition

  def fromEntityDefinition(e: omf#ModelEntityDefinition)
  : omf#IRI

  // entity concept

  /**
    * @param c A concept
    * @return A tuple consisting of:
    *         - the IRI of the concept
    *         - if any, the IRI of the graph corresponding to the concept
    *         - a boolean flag indicating whether this is an abstract concept or not
    * @since 0.10.3
    */
  def fromEntityConcept
  (c: omf#ModelEntityConcept)
  : EntityConceptSignature[omf]

  def equivalentEntityConcepts
  (c1: Iterable[omf#ModelEntityConcept], c2: Iterable[omf#ModelEntityConcept])
  : Boolean = {
    val iris1 = c1.map(fromEntityConcept).toSet
    val iris2 = c2.map(fromEntityConcept).toSet
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
  def fromEntityReifiedRelationship
  (r: omf#ModelEntityReifiedRelationship)
  : EntityReifiedRelationshipSignature[omf]

  def fromEntityUnreifiedRelationship
  (r: omf#ModelEntityUnreifiedRelationship)
  : EntityUnreifiedRelationshipSignature[omf]

  /**
    * Compares the relationships in terms of their sources, target & characteristics
    * Does not compare the graphs corresponding to each relationship, if any	.
    *
    * @since 0.10.3
    */
  def equivalentEntityReifiedRelationships
  (r1: Iterable[omf#ModelEntityReifiedRelationship],
   r2: Iterable[omf#ModelEntityReifiedRelationship])
  : Boolean = {
    val left = r1.map { r =>
      val s = fromEntityReifiedRelationship(r)
      (s.iri,
        fromEntityDefinition(s.source),
        fromEntityDefinition(s.target),
        relationshipCharacteristicsSummary(s.characteristics))
    }
      .toSet
    val right = r2.map { r =>
      val s = fromEntityReifiedRelationship(r)
      (s.iri,
        fromEntityDefinition(s.source),
        fromEntityDefinition(s.target),
        relationshipCharacteristicsSummary(s.characteristics))
    }
      .toSet
    val d = left.diff(right)
    d.isEmpty
  }

  // datatype definition

  def fromDataTypeDefinition
  (dt: omf#ModelDataTypeDefinition)
  : (UUID, LocalName, omf#IRI)

  // scalar datatype

  def fromScalarDataType
  (dt: omf#ModelScalarDataType)
  : (UUID, LocalName, omf#IRI)

  def equivalentScalarDataTypes
  (dt1: Iterable[omf#ModelScalarDataType],
   dt2: Iterable[omf#ModelScalarDataType])
  : Boolean = {
    val left = dt1.map(fromScalarDataType).toSet
    val right = dt2.map(fromScalarDataType).toSet
    val d = left.diff(right)
    d.isEmpty
  }

  // structured datatype

  def fromStructuredDataType
  (dt: omf#ModelStructuredDataType)
  : (UUID, LocalName, omf#IRI)

  def equivalentStructuredDataTypes
  (dt1: Iterable[omf#ModelStructuredDataType],
   dt2: Iterable[omf#ModelStructuredDataType])
  : Boolean = {
    val left = dt1.map(fromStructuredDataType).toSet
    val right = dt2.map(fromStructuredDataType).toSet
    val d = left.diff(right)
    d.isEmpty
  }

  // data relationship from entity to scalar

  def fromDataRelationshipFromEntityToScalar
  (esc: omf#ModelDataRelationshipFromEntityToScalar)
  : (UUID, LocalName, omf#IRI, omf#ModelEntityDefinition, omf#ModelScalarDataType)

  // data relationship from entity to structure

  def fromDataRelationshipFromEntityToStructure
  (est: omf#ModelDataRelationshipFromEntityToStructure)
  : (UUID, LocalName, omf#IRI, omf#ModelEntityDefinition, omf#ModelStructuredDataType)

  // data relationship from structure to scalar

  def fromDataRelationshipFromStructureToScalar
  (esc: omf#ModelDataRelationshipFromStructureToScalar)
  : (UUID, LocalName, omf#IRI, omf#ModelStructuredDataType, omf#ModelScalarDataType)

  // data relationship from structure to structure

  def fromDataRelationshipFromStructureToStructure
  (est: omf#ModelDataRelationshipFromStructureToStructure)
  : (UUID, LocalName, omf#IRI, omf#ModelStructuredDataType, omf#ModelStructuredDataType)

  // model term axioms

  def foldTermAxiom[T]
  (funEntityDefinitionAspectSubClassAxiom
   : omf#EntityDefinitionAspectSubClassAxiom => T,
   funEntityConceptDesignationTerminologyGraphAxiom
   : omf#EntityConceptDesignationTerminologyGraphAxiom => T,
   funEntityConceptSubClassAxiom
   : omf#EntityConceptSubClassAxiom => T,
   funEntityDefinitionRestrictionAxiom
   : omf#EntityDefinitionRestrictionAxiom => T,
   funEntityReifiedRelationshipSubClassAxiom
   : omf#EntityReifiedRelationshipSubClassAxiom => T,
   funScalarDataTypeFacetRestrictionAxiom
   : omf#ScalarDataTypeFacetRestrictionAxiom => T,
   funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
   : omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral => T)
  (t: omf#ModelTermAxiom)
  : T

  def foldTerminologyGraphAxiom[T]
  (funTerminologyGraphDirectExtensionAxiom
   : omf#TerminologyGraphDirectExtensionAxiom => T,
   funTerminologyGraphDirectNestingAxiom
   : omf#TerminologyGraphDirectNestingAxiom => T)
  (t: omf#TerminologyGraphAxiom)
  : T

  // scalar data relationship restriction axiom from entity to literal

  /**
    * Accessor for ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
    *
    * The normative OWL2-DL semantics of this axiom is (in OWL2 functional syntax):
    * {{SubClassOf(
    * <entity definition>,
    * DataAllValuesFrom(<data relationship from entity to scalar>, DataOneOf(<literal>))}}
    *
    * @param ax ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
    * @return A quad of:
    *         - an axiom UUID
    *         - an entity definition (domain of the restriction)
    *         - a data relationship from entity to scalar data type (the restricted relationship)
    *         - the lexical representation of a literal value for the scalar data type (range of the restriction)
    */
  def fromModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (ax: omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral)
  : (UUID, omf#ModelEntityDefinition, omf#ModelDataRelationshipFromEntityToScalar, String)

  // entity definition aspect subclass axiom

  def fromEntityDefinitionAspectSubClassAxiom
  (ax: omf#EntityDefinitionAspectSubClassAxiom)
  : (UUID, omf#ModelEntityDefinition, omf#ModelEntityAspect)

  // entity concept designation terminology graph axiom

  def fromEntityConceptDesignationTerminologyGraphAxiom
  (ax: omf#EntityConceptDesignationTerminologyGraphAxiom)
  : (UUID, omf#ModelEntityConcept, omf#ModelTerminologyGraph)


  // entity concept subclass axiom

  def fromEntityConceptSubClassAxiom
  (ax: omf#EntityConceptSubClassAxiom)
  : (UUID, omf#ModelEntityConcept, omf#ModelEntityConcept)

  // entity concept restriction axiom

  def fromEntityDefinitionRestrictionAxiom
  (ax: omf#EntityDefinitionRestrictionAxiom)
  : (UUID, omf#ModelEntityDefinition, omf#ModelEntityReifiedRelationship, omf#ModelEntityDefinition, RestrictionKind)

  // entity relationship subclass axiom

  def fromEntityReifiedRelationshipSubClassAxiom
  (ax: omf#EntityReifiedRelationshipSubClassAxiom)
  : (UUID, omf#ModelEntityReifiedRelationship, omf#ModelEntityReifiedRelationship)

  // scalar datatype facet restriction axiom

  def fromScalarDataTypeFacetRestrictionAxiom
  (ax: omf#ScalarDataTypeFacetRestrictionAxiom)
  : (UUID,
    omf#ModelScalarDataType,
    omf#ModelScalarDataType,
    Iterable[FundamentalFacet],
    Iterable[ConstrainingFacet])

}

object ImmutableTerminologyGraphOps {

  def lookupEntityDefinitionScalarDataRelationshipRestrictions[omf <: OMF]
  (ops: ImmutableTerminologyGraphOps[omf],
   graph: omf#ModelTerminologyGraph,
   entity: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]
  = {
    val axioms = for {
      ax <- ops.getTermAxioms(graph)._2
      e2l <- ops.foldTermAxiom[Option[omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]](
        funEntityDefinitionAspectSubClassAxiom =
          (_: omf#EntityDefinitionAspectSubClassAxiom) => None,
        funEntityConceptDesignationTerminologyGraphAxiom =
          (_: omf#EntityConceptDesignationTerminologyGraphAxiom) => None,
        funEntityConceptSubClassAxiom =
          (_: omf#EntityConceptSubClassAxiom) => None,
        funEntityDefinitionRestrictionAxiom =
          (_: omf#EntityDefinitionRestrictionAxiom) => None,
        funEntityReifiedRelationshipSubClassAxiom =
          (_: omf#EntityReifiedRelationshipSubClassAxiom) => None,
        funScalarDataTypeFacetRestrictionAxiom =
          (_: omf#ScalarDataTypeFacetRestrictionAxiom) => None,
        funModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =
          (x: omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral) =>
          if (entity == ops.fromModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(x)._2)
            Some(x)
          else
            None
      )(ax)
    } yield e2l
    axioms.to[Set]
  }

}

trait MutableTerminologyGraphOps[omf <: OMF]
  extends ImmutableTerminologyGraphOps[omf] {
  self: OMFStoreOps[omf] with IRIOps[omf] =>

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
  protected def addEntityAspect
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   aspectName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityAspect

  /**
    * Add to a terminology graph a new OMF Aspect
    * with a version 5 UUID based on the `graph` IRI and `aspectName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Aspect
    *
    * @param graph      : a terminology graph
    * @param aspectName : the name of a new entity aspect
    */
  final def addEntityAspect
  (graph: omf#MutableModelTerminologyGraph,
   aspectName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityAspect
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri, s"Aspect($aspectName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityAspect(graph, uuid, aspectName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF Concept.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Concept
    * @param graph
    * @param uuid
    * @param conceptName
    * @param isAbstract
    * @param store
    * @return
    */
  protected def addEntityConcept
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   conceptName: LocalName,
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityConcept

  /**
    * Add to a terminology graph a new OMF Concept
    * with a version 5 UUID based on the `graph` IRI and `conceptName`.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.Concept
    *
    * @param graph       : a terminology graph
    * @param conceptName : the name of a new entity concept
    * @param isAbstract  : boolean flag
    */
  final def addEntityConcept
  (graph: omf#MutableModelTerminologyGraph,
   conceptName: LocalName,
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityConcept
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri, s"Concept($conceptName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityConcept(graph, uuid, conceptName, isAbstract)
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
    * @param isAbstract
    * @param store
    * @return
    */
  protected def addEntityReifiedRelationship
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   source: omf#ModelEntityDefinition,
   target: omf#ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityReifiedRelationship

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
    * @param isAbstract                       boolean flag
    */
  final def addEntityReifiedRelationship
  (graph: omf#MutableModelTerminologyGraph,
   source: omf#ModelEntityDefinition,
   target: omf#ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelEntityReifiedRelationship
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri, s"ReifiedRelationship($reifiedRelationshipName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityReifiedRelationship(
      graph, uuid, source, target, characteristics,
      reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName,
      isAbstract)
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
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   scalarDataTypeName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelScalarDataType

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
  (graph: omf#MutableModelTerminologyGraph,
   scalarDataTypeName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelScalarDataType
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri, s"Scalar($scalarDataTypeName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addScalarDataType(graph, uuid, scalarDataTypeName)
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
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   structureDatatypeName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelStructuredDataType

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
  (graph: omf#MutableModelTerminologyGraph,
   structureDatatypeName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelStructuredDataType
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri, s"Structure($structureDatatypeName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addStructuredDataType(graph, uuid, structureDatatypeName)
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
    * @param store
    * @return
    */
  protected def addDataRelationshipFromEntityToScalar
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   source: omf#ModelEntityDefinition,
   target: omf#ModelScalarDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToScalar

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
    * @param store
    * @return
    */
  final def addDataRelationshipFromEntityToScalar
  (graph: omf#MutableModelTerminologyGraph,
   source: omf#ModelEntityDefinition,
   target: omf#ModelScalarDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToScalar
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"EntityScalarDataProperty(${fromIRI(fromEntityDefinition(source))},$dataPropertyName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addDataRelationshipFromEntityToScalar(graph, uuid, source, target, dataPropertyName)
  } yield ax

  /**
    * Add to a terminology graph a new OMF EntityScalarDataPropertyParticularRestrictionAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.EntityScalarDataPropertyParticularRestrictionAxiom
    *
    * @param graph
    * @param uuid
    * @param entityDomain
    * @param scalarDataProperty
    * @param literalRange
    * @param store
    * @return
    */
  protected def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   entityDomain: omf#ModelEntityDefinition,
   scalarDataProperty: omf#ModelDataRelationshipFromEntityToScalar,
   literalRange: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral

  def scalarDataRelationshipRestrictionAxiomFromEntityToLiteralUUID
  (graph: omf#MutableModelTerminologyGraph,
   entityDomain: omf#ModelEntityDefinition,
   scalarDataProperty: omf#ModelDataRelationshipFromEntityToScalar)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminologyGraph(graph).iri,
      s"ScalarRangeAxiom("+
        fromIRI(fromEntityDefinition(entityDomain))+","+
        fromIRI(fromDataRelationshipFromEntityToScalar(scalarDataProperty)._3)+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

  /**
    * Add to a terminology graph a new OMF EntityScalarDataPropertyParticularRestrictionAxiom.
    * with a version 5 UUID based on the `graph`, `entityDomain` and `scalaDataProperty` IRIs
    * and the `literalRange`.
    *
    * @param graph
    * @param entityDomain
    * @param scalarDataProperty
    * @param literalRange
    * @param store
    * @return
    */
  final def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (graph: omf#MutableModelTerminologyGraph,
   entityDomain: omf#ModelEntityDefinition,
   scalarDataProperty: omf#ModelDataRelationshipFromEntityToScalar,
   literalRange: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = for {
    uuid <- scalarDataRelationshipRestrictionAxiomFromEntityToLiteralUUID(graph, entityDomain, scalarDataProperty)
    ax <- addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
      graph, uuid, entityDomain, scalarDataProperty, literalRange)
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
    * @param store
    * @return
    */
  protected def addDataRelationshipFromEntityToStructure
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   source: omf#ModelEntityDefinition,
   target: omf#ModelStructuredDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToStructure

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
    * @param store
    * @return
    */
  final def addDataRelationshipFromEntityToStructure
  (graph: omf#MutableModelTerminologyGraph,
   source: omf#ModelEntityDefinition,
   target: omf#ModelStructuredDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToStructure
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"EntityStructuredDataProperty(${fromIRI(fromEntityDefinition(source))},$dataPropertyName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addDataRelationshipFromEntityToStructure(graph, uuid, source, target, dataPropertyName)
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
  protected def addDataRelationshipFromStructureToScalar
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   source: omf#ModelStructuredDataType,
   target: omf#ModelScalarDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToScalar

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
  final def addDataRelationshipFromStructureToScalar
  (graph: omf#MutableModelTerminologyGraph,
   source: omf#ModelStructuredDataType,
   target: omf#ModelScalarDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToScalar
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ScalarDataProperty(${fromIRI(fromStructuredDataType(source)._3)},$dataPropertyName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addDataRelationshipFromStructureToScalar(graph, uuid, source, target, dataPropertyName)
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
  protected def addDataRelationshipFromStructureToStructure
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   source: omf#ModelStructuredDataType,
   target: omf#ModelStructuredDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToStructure

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
  final def addDataRelationshipFromStructureToStructure
  (graph: omf#MutableModelTerminologyGraph,
   source: omf#ModelStructuredDataType,
   target: omf#ModelStructuredDataType,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToStructure
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"StructuredDataProperty(${fromIRI(fromStructuredDataType(source)._3)},$dataPropertyName)")
    uuid = generateUUID(fromIRI(iri))
    ax <- addDataRelationshipFromStructureToStructure(graph, uuid, source, target, dataPropertyName)
  } yield ax

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
  protected def addEntityDefinitionAspectSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   sub: omf#ModelEntityDefinition,
   sup: omf#ModelEntityAspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionAspectSubClassAxiom

  def aspectSpecializationAxiomUUID
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   sup: omf#ModelEntityAspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"AspectSpecializationAxiom("+
        fromIRI(fromEntityDefinition(sub))+","+
        fromIRI(fromEntityAspect(sup))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  final def addEntityDefinitionAspectSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   sup: omf#ModelEntityAspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionAspectSubClassAxiom
  = for {
    uuid <- aspectSpecializationAxiomUUID(graph, sub, sup)
    ax <- addEntityDefinitionAspectSubClassAxiom(graph, uuid, sub, sup)
  } yield ax

  def conceptSpecializationAxiomUUID
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityConcept,
   sup: omf#ModelEntityConcept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ConceptSpecializationAxiom("+
        fromIRI(fromEntityConcept(sub).iri)+","+
        fromIRI(fromEntityConcept(sup).iri)+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  protected def addEntityConceptSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   sub: omf#ModelEntityConcept,
   sup: omf#ModelEntityConcept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityConceptSubClassAxiom

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
  final def addEntityConceptSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityConcept,
   sup: omf#ModelEntityConcept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityConceptSubClassAxiom
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ConceptSpecializationAxiom(${fromIRI(fromEntityConcept(sub).iri)}, ${fromIRI(fromEntityConcept(sup).iri)}")
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityConceptSubClassAxiom(graph, uuid, sub, sup)
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
  protected def addEntityDefinitionUniversalRestrictionAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionUniversalRestrictionAxiom

  def entityUniversalRestrictionAxiomUUID
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"EntityUniversalRestrictionAxiom("+
        fromIRI(fromEntityDefinition(sub))+","+
        fromIRI(fromEntityReifiedRelationship(rel).iri)+","+
        fromIRI(fromEntityDefinition(range))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  final def addEntityDefinitionUniversalRestrictionAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(graph, sub, rel, range)
    ax <- addEntityDefinitionUniversalRestrictionAxiom(graph, uuid, sub, rel, range)
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
  protected def addEntityDefinitionExistentialRestrictionAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionExistentialRestrictionAxiom

  def entityExistentialRestrictionAxiomUUID
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"EntityExistentialRestrictionAxiom("+
        fromIRI(fromEntityDefinition(sub))+","+
        fromIRI(fromEntityReifiedRelationship(rel).iri)+","+
        fromIRI(fromEntityDefinition(range))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  final def addEntityDefinitionExistentialRestrictionAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityDefinition,
   rel: omf#ModelEntityReifiedRelationship,
   range: omf#ModelEntityDefinition)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityDefinitionExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(graph, sub, rel, range)
    ax <- addEntityDefinitionExistentialRestrictionAxiom(graph, uuid, sub, rel, range)
  } yield ax

  /**
    * Add to a terminology graph a new ReifiedRelationshipSpecializationAxiom.
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
  protected def addEntityReifiedRelationshipSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   sub: omf#ModelEntityReifiedRelationship,
   sup: omf#ModelEntityReifiedRelationship)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityReifiedRelationshipSubClassAxiom

  def reifiedRelationshipSubClassAxiomUUID
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityReifiedRelationship,
   sup: omf#ModelEntityReifiedRelationship)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ReifiedRelationshipSpecializationAxiom(" +
        fromIRI(fromEntityReifiedRelationship(sub).iri) + "," +
        fromIRI(fromEntityReifiedRelationship(sup).iri) + ")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

  /**
    * Add to a terminology graph a new ReifiedRelationshipSpecializationAxiom
    * with a version 5 UUID based on the `graph`, `sub` and `sup` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ReifiedRelationshipSpecializationAxiom
    *
    * @param graph
    * @param sub
    * @param sup
    * @param store
    * @return
    */
  final def addEntityReifiedRelationshipSubClassAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelEntityReifiedRelationship,
   sup: omf#ModelEntityReifiedRelationship)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityReifiedRelationshipSubClassAxiom
  = for {
    uuid <- reifiedRelationshipSubClassAxiomUUID(graph,sub,sup)
    ax <- addEntityReifiedRelationshipSubClassAxiom(graph, uuid, sub, sup)
  } yield ax

  protected def addScalarDataTypeFacetRestrictionAxiom
  (uuid: UUID,
   graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelScalarDataType,
   sup: omf#ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet])
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ScalarDataTypeFacetRestrictionAxiom

  /**
    * Needs to be refactored in one of the OMF ScalarRestrictionAxioms:
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.BinaryScalarRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.IRIScalarRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.NumericScalarRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.PlainLiteralScalarRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ScalarOneOfRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.StringScalarRestrictionAxiom
    * - https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TimeScalarRestrictionAxiom
    *
    * @param graph
    * @param sub
    * @param sup
    * @param fundamentalFacets
    * @param constrainingFacets
    * @param store
    * @return
    */
  final def addScalarDataTypeFacetRestrictionAxiom
  (graph: omf#MutableModelTerminologyGraph,
   sub: omf#ModelScalarDataType,
   sup: omf#ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet])
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ScalarDataTypeFacetRestrictionAxiom
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ScalarDataTypeFacetRestrictionAxiom("+
        fromIRI(fromScalarDataType(sup)._3)+","+
        fromIRI(fromScalarDataType(sup)._3)+")")
    uuid = generateUUID(fromIRI(iri))
    ax <- addScalarDataTypeFacetRestrictionAxiom(
      uuid, graph, sub, sup,fundamentalFacets,constrainingFacets)
  } yield ax

  /**
    * Add to a terminology graph a new OMF TerminologyExtensionAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyExtensionAxiom
    *
    * @param extendingG
    * @param uuid
    * @param extendedG
    * @param store
    * @return
    */
  protected def addTerminologyGraphExtension
  (uuid: UUID,
   extendingG: omf#MutableModelTerminologyGraph,
   extendedG: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyGraphDirectExtensionAxiom

  def terminologyGraphExtensionUUID
  (extendingG: omf#MutableModelTerminologyGraph,
   extendedG: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminologyGraph(extendingG).iri,
      s"TerminologyGraphExtensionAxiom(${fromIRI(fromTerminologyGraph(extendedG).iri)})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  final def addTerminologyGraphExtension
  (extendingG: omf#MutableModelTerminologyGraph,
   extendedG: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyGraphDirectExtensionAxiom
  = for {
    uuid <- terminologyGraphExtensionUUID(extendingG, extendedG)
    ax <- addTerminologyGraphExtension(uuid, extendingG, extendedG)
  } yield ax

  /**
    * Add to a terminology graph a new OMF TerminologyNestingAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.TerminologyNestingAxiom
    *
    * @param uuid
    * @param nestingGraph
    * @param nestingContext
    * @param store
    * @return
    */
  protected def addNestedTerminologyGraph
  (uuid: UUID,
   nestingGraph: omf#ModelTerminologyGraph,
   nestingContext: omf#ModelEntityConcept,
   nestedGraph: omf#MutableModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyGraphDirectNestingAxiom

  def terminologyNestingAxiomUUID
  (nestingContext: omf#ModelEntityConcept,
   nestedGraph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminologyGraph(nestedGraph).iri,
      s"TerminologyNestingAxiom(${fromEntityConcept(nestingContext).name})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  final def addNestedTerminologyGraph
  (nestingGraph: omf#ModelTerminologyGraph,
   nestingContext: omf#ModelEntityConcept,
   nestedGraph: omf#MutableModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyGraphDirectNestingAxiom
  = for {
    uuid <- terminologyNestingAxiomUUID(nestingContext, nestedGraph)
    ax <- addNestedTerminologyGraph(uuid, nestingGraph, nestingContext, nestedGraph)
  } yield ax

  /**
    * Add to a terminology graph a new OMF ConceptDesignationTerminologyGraphAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptDesignationTerminologyGraphAxiom
    *
    * @param graph                       The mutable terminology graph in which to assert the axiom
    * @param entityConceptDesignation    The model entity concept whose complete complete designation is specified
    * @param designationTerminologyGraph The terminology graph specifying the complete designation
    *                                    for the structural contents of the model entity concept
    * @param store                       OMF storage provider
    * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
    */
  protected def addEntityConceptDesignationTerminologyGraphAxiom
  (graph: omf#MutableModelTerminologyGraph,
   uuid: UUID,
   entityConceptDesignation: omf#ModelEntityConcept,
   designationTerminologyGraph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityConceptDesignationTerminologyGraphAxiom

  /**
    * Add to a terminology graph a new OMF ConceptDesignationTerminologyGraphAxiom
    * with a version 5 UUID based on `graph`, `entityConceptDesignation` and `designationTerminologyGraph` IRIs.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptDesignationTerminologyGraphAxiom
    *
    * @param graph                       The mutable terminology graph in which to assert the axiom
    * @param entityConceptDesignation    The model entity concept whose complete designation is specified
    * @param designationTerminologyGraph The terminology graph specifying the complete designation
    *                                    for the structural contents of the model entity concept
    * @param store                       OMF storage provider
    * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
    */
  final def addEntityConceptDesignationTerminologyGraphAxiom
  (graph: omf#MutableModelTerminologyGraph,
   entityConceptDesignation: omf#ModelEntityConcept,
   designationTerminologyGraph: omf#ModelTerminologyGraph)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityConceptDesignationTerminologyGraphAxiom
  = for {
    iri <- withFragment(
      fromTerminologyGraph(graph).iri,
      s"ConceptDesignationTerminologyGraphAxiom("+
        fromEntityConcept(entityConceptDesignation).name+","+
        fromIRI(fromTerminologyGraph(designationTerminologyGraph).iri)+
        ")")
    ax <- addEntityConceptDesignationTerminologyGraphAxiom(
      graph,
      generateUUID(fromIRI(iri)),
      entityConceptDesignation,
      designationTerminologyGraph)
  } yield ax

}

trait ImmutableInstanceGraphOps[omf <: OMF] {

  def getInstanceGraphIRI
  (graph: omf#ModelInstanceGraph)
  : omf#IRI

  // instance object

  def fromInstanceObject
  (o: omf#ModelInstanceObject)
  : (omf#IRI, omf#ModelEntityConcept)

  // instance relation

  def fromInstanceRelation
  (r: omf#ModelInstanceRelation)
  : (omf#IRI, omf#ModelEntityReifiedRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance)

  // data literal

  def fromDataLiteral
  (dl: omf#ModelInstanceDataLiteral)
  : (String, omf#ModelScalarDataType)

  // data structure

  def fromDataStructure
  (ds: omf#ModelInstanceDataStructure)
  : (omf#IRI, omf#ModelStructuredDataType)

  // data relationship from entity to scalar

  def fromInstanceDataRelationshipFromEntityToScalar
  (e2sc: omf#ModelInstanceDataRelationshipFromEntityToScalar)
  : (omf#ModelEntityInstance,
    omf#ModelDataRelationshipFromEntityToScalar,
    omf#ModelInstanceDataLiteral)

  // data relationship from entity to structure

  def fromInstanceDataRelationshipFromEntityToStructure
  (e2sc: omf#ModelInstanceDataRelationshipFromEntityToStructure)
  : (omf#ModelEntityInstance,
    omf#ModelDataRelationshipFromEntityToStructure,
    omf#ModelInstanceDataStructure)

  // data relationship from structure to scalar

  def fromInstanceDataRelationshipFromStructureToScalar
  (e2sc: omf#ModelInstanceDataRelationshipFromStructureToScalar)
  : (omf#ModelInstanceDataStructure,
    omf#ModelDataRelationshipFromStructureToScalar,
    omf#ModelInstanceDataLiteral)

  // data relationship from structure to structure

  def fromInstanceDataRelationshipFromStructureToStructure
  (e2sc: omf#ModelInstanceDataRelationshipFromStructureToStructure)
  : (omf#ModelInstanceDataStructure,
    omf#ModelDataRelationshipFromStructureToStructure,
    omf#ModelInstanceDataStructure)

}

trait MutableInstanceGraphOps[omf <: OMF]
  extends ImmutableInstanceGraphOps[omf] {

  // instance object

  def addInstanceObject
  (graph: omf#MutableModelInstanceGraph,
   conceptType: omf#ModelEntityConcept,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceObject

  // instance relation

  def addInstanceRelation
  (graph: omf#MutableModelInstanceGraph,
   relationshipType: omf#ModelEntityReifiedRelationship,
   source: omf#ModelEntityInstance,
   target: omf#ModelEntityInstance,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceRelation

  // data literal

  def addDataLiteral
  (graph: omf#MutableModelInstanceGraph,
   datatype: omf#ModelScalarDataType,
   lexicalForm: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataLiteral

  // data structure

  def addDataStructure
  (graph: omf#MutableModelInstanceGraph,
   datatype: omf#ModelStructuredDataType,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataStructure

  // data relationship from entity to scalar

  def addInstanceDataRelationshipFromEntityToScalar
  (graph: omf#MutableModelInstanceGraph,
   ei: omf#ModelEntityInstance,
   e2sc: omf#ModelDataRelationshipFromEntityToScalar,
   value: omf#ModelInstanceDataLiteral)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToScalar

  // data relationship from entity to structure

  def addInstanceDataRelationshipFromEntityToStructure
  (graph: omf#MutableModelInstanceGraph,
   ei: omf#ModelEntityInstance,
   e2st: omf#ModelDataRelationshipFromEntityToStructure,
   value: omf#ModelInstanceDataStructure)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToStructure

  // data relationship from structure to scalar

  def addInstanceDataRelationshipFromStructureToScalar
  (graph: omf#MutableModelInstanceGraph,
   di: omf#ModelInstanceDataStructure,
   e2sc: omf#ModelDataRelationshipFromStructureToScalar,
   value: omf#ModelInstanceDataLiteral)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToScalar

  // data relationship from structure to structure

  def addInstanceDataRelationshipFromStructureToStructure
  (graph: omf#MutableModelInstanceGraph,
   di: omf#ModelInstanceDataStructure,
   e2st: omf#ModelDataRelationshipFromStructureToStructure,
   value: omf#ModelInstanceDataStructure)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToStructure

}

trait OMFOps[omf <: OMF]
  extends IRIOps[omf]
    with MutableTerminologyGraphOps[omf]
    with MutableInstanceGraphOps[omf]
    with OMFStoreOps[omf]