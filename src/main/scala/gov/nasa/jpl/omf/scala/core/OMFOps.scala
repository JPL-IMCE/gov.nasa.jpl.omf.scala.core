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

import gov.nasa.jpl.imce.omf.schema.tables.{LexicalValue, LocalName}
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

import scala.{Boolean, Int, None, Option, Some, StringContext, Unit}
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

  def foldTerminology[T]
  (funImmutableTerminologyGraph: omf#ImmutableTerminologyGraph => T,
   funMutableTerminologyGraph: omf#MutableTerminologyGraph => T,
   funImmutableTerminologyBundle: omf#ImmutableBundle => T,
   funMutableTerminologyBundle: omf#MutableBundle => T)
  (t: omf#TerminologyBox)
  : T

  def foldTerminologyGraph
  (t: omf#TerminologyBox)
  : Option[omf#TerminologyGraph]
  = foldTerminology[Option[omf#TerminologyGraph]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None
  )(t)

  def foldMutableTerminologyGraph
  (t: omf#TerminologyBox)
  : Option[omf#MutableTerminologyGraph]
  = foldTerminology[Option[omf#MutableTerminologyGraph]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (g: omf#MutableTerminologyGraph) => Some(g),
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None
  )(t)


  def foldImmutableTerminologyGraph
  (t: omf#TerminologyBox)
  : Option[omf#ImmutableTerminologyGraph]
  = foldTerminology[Option[omf#ImmutableTerminologyGraph]](
    funImmutableTerminologyGraph = (g: omf#ImmutableTerminologyGraph) => Some(g),
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None
  )(t)


  def foldBundle
  (t: omf#TerminologyBox)
  : Option[omf#Bundle]
  = foldTerminology[Option[omf#Bundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b)
  )(t)

  def foldMutableBundle
  (t: omf#TerminologyBox)
  : Option[omf#MutableBundle]
  = foldTerminology[Option[omf#MutableBundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (_: omf#ImmutableBundle) => None,
    funMutableTerminologyBundle = (b: omf#MutableBundle) => Some(b)
  )(t)

  def foldImmutableBundle
  (t: omf#TerminologyBox)
  : Option[omf#ImmutableBundle]
  = foldTerminology[Option[omf#ImmutableBundle]](
    funImmutableTerminologyGraph = (_: omf#ImmutableTerminologyGraph) => None,
    funMutableTerminologyGraph = (_: omf#MutableTerminologyGraph) => None,
    funImmutableTerminologyBundle = (b: omf#ImmutableBundle) => Some(b),
    funMutableTerminologyBundle = (_: omf#MutableBundle) => None
  )(t)

  def lookupTerminology
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Option[omf#TerminologyBox]

  def lookupTerminology
  (uuid: UUID)
  (implicit store: omf#Store)
  : Option[omf#TerminologyBox]

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
  : Set[java.lang.Throwable] \/ (omf#ImmutableTerminologyBox, omf#Mutable2ImmutableTerminologyMap)

  def loadTerminology
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ (omf#ImmutableTerminologyBox, omf#Mutable2ImmutableTerminologyMap)

  def isTerminologyMutable
  (graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Boolean

  def asMutableTerminology
  (graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Option[omf#MutableTerminologyBox]

  def isTerminologyImmutable
  (graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Boolean

  def asImmutableTerminology
  (graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Option[omf#ImmutableTerminologyBox]

  def fromTerminology
  (graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : TerminologySignature[omf]

  def getTerminologyAxiomUUID
  (ax: omf#TerminologyAxiom)
  (implicit store: omf#Store)
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
  : Set[java.lang.Throwable] \/ omf#MutableTerminologyGraph

  protected def makeBundle
  (uuid: UUID,
   name: LocalName,
   iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#MutableBundle

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
  : Set[java.lang.Throwable] \/ omf#MutableTerminologyGraph
  = for {
    name <- lastSegment(iri)
    g <- makeTerminologyGraph(generateUUID(fromIRI(iri)+"#TerminologyGraph"), name, iri, kind)
  } yield g

  def makeBundle
  (iri: omf#IRI,
   kind: TerminologyKind)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#MutableBundle
  = for {
    name <- lastSegment(iri)
    g <- makeBundle(generateUUID(fromIRI(iri)+"#Bundle"), name, iri, kind)
  } yield g

  def saveTerminology
  (g: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ Unit

  def saveTerminology
  (g: omf#TerminologyBox,
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
  def asImmutableTerminology
  (g: omf#MutableTerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableTerminologyBox, omf#Mutable2ImmutableTerminologyMap)

  def asImmutableTerminology
  (m2i: omf#Mutable2ImmutableTerminologyMap,
   g: omf#MutableTerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/
    (omf#ImmutableTerminologyBox, omf#Mutable2ImmutableTerminologyMap)

  def isTermAssertedInTerminology
  (t: omf#Term, graph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Boolean
  = fromTerminology(graph)
    .terms
    .exists(_ == t)

  def isTermImportedInTerminology
  (t: omf#Term, graph: omf#TerminologyBox)
  (implicit ops: OMFOps[omf], store: omf#Store)
  : Boolean
  = terminologyImportClosure[omf, omf#TerminologyBox](graph)
    .exists(isTermAssertedInTerminology(t, _))

  def loadInstanceGraph
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ImmutableModelInstanceGraph

  def fromInstanceGraph
  (graph: omf#ModelInstanceGraph)
  : (omf#IRI,
    Iterable[omf#ImmutableTerminologyBox],
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
   instantiatedTGraphs: Iterable[omf#ImmutableTerminologyBox],
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

  def resolveIRIAsLocalFile
  (iri: omf#IRI)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ File
}

trait ImmutableTerminologyGraphOps[omf <: OMF] { self: OMFStoreOps[omf] with IRIOps[omf] =>

  def getTerminologyIRI
  (graph: omf#TerminologyBox)
  : omf#IRI

  def getTerminologyName
  (graph: omf#TerminologyBox)
  : LocalName

  def getTerminologyUUID
  (graph: omf#TerminologyBox)
  : UUID

  def getTerminologyKind
  (graph: omf#TerminologyBox)
  : TerminologyKind

  def lookupTerm
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Term]

  def lookupEntity
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Entity]

  def lookupAspect
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Aspect]

  def lookupConcept
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Concept]

  def lookupReifiedRelationship
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ReifiedRelationship]

  def lookupUnreifiedRelationship
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#UnreifiedRelationship]

  def lookupDataRange
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#DataRange]

  def lookupStructure
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#Structure]

  def lookupEntityScalarDataProperty
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#EntityScalarDataProperty]

  def lookupEntityStructuredDataProperty
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#EntityStructuredDataProperty]

  def lookupScalarDataProperty
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#ScalarDataProperty]

  def lookupStructuredDataProperty
  (graph: omf#TerminologyBox, iri: omf#IRI, recursively: Boolean)
  (implicit store: omf#Store)
  : Option[omf#StructuredDataProperty]

  def getAxiomUUID
  (ax: omf#Axiom)
  : UUID

  def getAxioms
  (graph: omf#TerminologyBox)
  : (omf#IRI, Iterable[omf#Axiom])

  def getTerms
  (graph: omf#TerminologyBox)
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
   funTimeScalarRestriction: omf#TimeScalarRestriction => T,
   funEntityScalarDataProperty: omf#EntityScalarDataProperty => T,
   funEntityStructuredDataProperty: omf#EntityStructuredDataProperty => T,
   funScalarDataProperty: omf#ScalarDataProperty => T,
   funStructuredDataProperty: omf#StructuredDataProperty => T)
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
  : EntityConceptSignature[omf]

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
  : EntityReifiedRelationshipSignature[omf]

  def fromUnreifiedRelationship
  (r: omf#UnreifiedRelationship)
  : EntityUnreifiedRelationshipSignature[omf]

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
  : AnonymousConceptTaxonomySignature[omf]

  def fromRootConceptTaxonomyAxiom
  (ax: omf#RootConceptTaxonomyAxiom)
  : RootConceptTaxonomySignature[omf]

  def fromSpecificDisjointConceptAxiom
  (ax: omf#SpecificDisjointConceptAxiom)
  : SpecificDisjointConceptSignature[omf]

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
  protected def addAspect
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   aspectName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#Aspect

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
  : Set[java.lang.Throwable] \/ omf#Aspect
  = for {
    iri <- withFragment(fromTerminology(graph).iri, aspectName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addAspect(graph, uuid, iri, aspectName)
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
  protected def addConcept
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   conceptName: LocalName,
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#Concept

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
  final def addConcept
  (graph: omf#MutableTerminologyBox,
   conceptName: LocalName,
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#Concept
  = for {
    iri <- withFragment(fromTerminology(graph).iri, conceptName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addConcept(graph, uuid, iri, conceptName, isAbstract)
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
  protected def addReifiedRelationship
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ReifiedRelationship

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
  final def addReifiedRelationship
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#Entity,
   characteristics: Iterable[RelationshipCharacteristics],
   reifiedRelationshipName: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   isAbstract: Boolean)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ReifiedRelationship
  = for {
    iri <- withFragment(fromTerminology(graph).iri, reifiedRelationshipName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addReifiedRelationship(
      graph, uuid, iri, source, target, characteristics,
      reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName,
      isAbstract)
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
  : Set[java.lang.Throwable] \/ omf#UnreifiedRelationship

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
  : Set[java.lang.Throwable] \/ omf#UnreifiedRelationship
  = for {
    iri <- withFragment(fromTerminology(graph).iri, unreifiedRelationshipName)
    uuid = generateUUID(fromIRI(iri))
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
  : Set[java.lang.Throwable] \/ omf#Scalar

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
  : Set[java.lang.Throwable] \/ omf#Scalar
  = for {
    iri <- withFragment(fromTerminology(graph).iri, scalarDataTypeName)
    uuid = generateUUID(fromIRI(iri))
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
  : Set[java.lang.Throwable] \/ omf#Structure

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
  : Set[java.lang.Throwable] \/ omf#Structure
  = for {
    iri <- withFragment(fromTerminology(graph).iri, structureDatatypeName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addStructuredDataType(graph, uuid, iri, structureDatatypeName)
  } yield ax

  protected def addScalarOneOfRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: omf#IRI,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ScalarOneOfRestriction

  final def addScalarOneOfRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ScalarOneOfRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addScalarOneOfRestriction(graph, dataTypeUUID, dataTypeIRI, dataTypeName, restrictedRange)
  } yield ax

  protected def addBinaryScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   dataTypeIRI: omf#IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#BinaryScalarRestriction

  final def addBinaryScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[Int]=None,
   minLength: Option[Int]=None,
   maxLength: Option[Int]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#BinaryScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addBinaryScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, restrictedRange)
  } yield ax

  protected def addIRIScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#IRIScalarRestriction

  final def addIRIScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[Int]=None,
   minLength: Option[Int]=None,
   maxLength: Option[Int]=None,
   pattern: Option[String]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#IRIScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addIRIScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, restrictedRange)
  } yield ax

  protected def addNumericScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#NumericScalarRestriction

  final def addNumericScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   minInclusive: Option[String]=None,
   maxInclusive: Option[String]=None,
   minExclusive: Option[String]=None,
   maxExclusive: Option[String]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#NumericScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addNumericScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      minInclusive, maxInclusive, minExclusive, maxExclusive, restrictedRange)
  } yield ax

  protected def addPlainLiteralScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   language: Option[String],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#PlainLiteralScalarRestriction

  final def addPlainLiteralScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[Int]=None,
   minLength: Option[Int]=None,
   maxLength: Option[Int]=None,
   pattern: Option[String]=None,
   language: Option[String]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#PlainLiteralScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addPlainLiteralScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, language, restrictedRange)
  } yield ax

  protected def addStringScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#StringScalarRestriction

  final def addStringScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   length: Option[Int]=None,
   minLength: Option[Int]=None,
   maxLength: Option[Int]=None,
   pattern: Option[String]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#StringScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
    ax <- addStringScalarRestriction(
      graph, dataTypeUUID, dataTypeIRI, dataTypeName,
      length, minLength, maxLength, pattern, restrictedRange)
  } yield ax

  protected def addTimeScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeUUID: UUID,
   iri: omf#IRI,
   dataTypeName: LocalName,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String],
   restrictedRange: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TimeScalarRestriction

  final def addTimeScalarRestriction
  (graph: omf#MutableTerminologyBox,
   dataTypeName: LocalName,
   restrictedRange: omf#DataRange,
   minInclusive: Option[String]=None,
   maxInclusive: Option[String]=None,
   minExclusive: Option[String]=None,
   maxExclusive: Option[String]=None)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TimeScalarRestriction
  = for {
    dataTypeIRI <- withFragment(fromTerminology(graph).iri, dataTypeName)
    dataTypeUUID = generateUUID(fromIRI(dataTypeIRI))
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
    * @param store
    * @return
    */
  protected def addEntityScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#DataRange,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataProperty

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
  final def addEntityScalarDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#DataRange,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataProperty
  = for {
    iri <- withFragment(fromTerminology(graph).iri,dataPropertyName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityScalarDataProperty(graph, uuid, iri, source, target, dataPropertyName)
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
  protected def addEntityStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   iri: omf#IRI,
   source: omf#Entity,
   target: omf#Structure,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityStructuredDataProperty

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
  final def addEntityStructuredDataProperty
  (graph: omf#MutableTerminologyBox,
   source: omf#Entity,
   target: omf#Structure,
   dataPropertyName: LocalName)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityStructuredDataProperty
  = for {
    iri <- withFragment(fromTerminology(graph).iri, dataPropertyName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addEntityStructuredDataProperty(graph, uuid, iri, source, target, dataPropertyName)
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
  : Set[java.lang.Throwable] \/ omf#ScalarDataProperty

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
  : Set[java.lang.Throwable] \/ omf#ScalarDataProperty
  = for {
    iri <- withFragment(fromTerminology(graph).iri, dataPropertyName)
    uuid = generateUUID(fromIRI(iri))
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
  : Set[java.lang.Throwable] \/ omf#StructuredDataProperty

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
  : Set[java.lang.Throwable] \/ omf#StructuredDataProperty
  = for {
    iri <- withFragment(fromTerminology(graph).iri, dataPropertyName)
    uuid = generateUUID(fromIRI(iri))
    ax <- addStructuredDataProperty(graph, uuid, iri, source, target, dataPropertyName)
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
  protected def addAspectSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#AspectSpecializationAxiom

  def aspectSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminology(graph).iri,
      s"AspectSpecializationAxiom("+
        fromIRI(getTermIRI(sub))+","+
        fromIRI(getTermIRI(sup))+")")
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
  final def addAspectSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   sup: omf#Aspect)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#AspectSpecializationAxiom
  = for {
    uuid <- aspectSpecializationAxiomUUID(graph, sub, sup)
    ax <- addAspectSpecializationAxiom(graph, uuid, sub, sup)
  } yield ax

  def conceptSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Concept,
   sup: omf#Concept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminology(graph).iri,
      s"ConceptSpecializationAxiom("+
        fromIRI(getTermIRI(sub))+","+
        fromIRI(getTermIRI(sup))+")")
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
  protected def addConceptSpecializationAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   sub: omf#Concept,
   sup: omf#Concept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ConceptSpecializationAxiom

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
  : Set[java.lang.Throwable] \/ omf#ConceptSpecializationAxiom
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"ConceptSpecializationAxiom(${fromIRI(getTermIRI(sub))}, ${fromIRI(getTermIRI(sup))}")
    uuid = generateUUID(fromIRI(iri))
    ax <- addConceptSpecializationAxiom(graph, uuid, sub, sup)
  } yield ax

  def reifiedRelationshipSpecializationAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#ReifiedRelationship,
   sup: omf#ReifiedRelationship)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"ReifiedRelationshipSpecializationAxiom("+
        fromIRI(getTermIRI(sub))+","+
        fromIRI(getTermIRI(sup))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  : Set[java.lang.Throwable] \/ omf#ReifiedRelationshipSpecializationAxiom

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
  : Set[java.lang.Throwable] \/ omf#ReifiedRelationshipSpecializationAxiom
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"ReifiedRelationshipSpecializationAxiom(${fromIRI(getTermIRI(sub))}, ${fromIRI(getTermIRI(sup))}")
    uuid = generateUUID(fromIRI(iri))
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
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityUniversalRestrictionAxiom

  def entityUniversalRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminology(graph).iri,
      s"EntityUniversalRestrictionAxiom("+
        fromIRI(getTermIRI(sub))+","+
        fromIRI(getTermIRI(rel))+","+
        fromIRI(getTermIRI(range))+")")
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
  final def addEntityUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityUniversalRestrictionAxiom
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
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityExistentialRestrictionAxiom

  def entityExistentialRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
     iri <- withFragment(
      fromTerminology(graph).iri,
      s"EntityExistentialRestrictionAxiom("+
        fromIRI(getTermIRI(sub))+","+
        fromIRI(getTermIRI(rel))+","+
        fromIRI(getTermIRI(range))+")")
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
  final def addEntityExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   sub: omf#Entity,
   rel: omf#ReifiedRelationship,
   range: omf#Entity)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityExistentialRestrictionAxiom
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
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyExistentialRestrictionAxiom

  def entityScalarDataPropertyExistentialRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"EntityScalarDataPropertyExistentialRestrictionAxiom("+
        fromIRI(getTermIRI(restrictedEntity))+","+
        fromIRI(getTermIRI(scalarProperty))+","+
        fromIRI(getTermIRI(range))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

  final def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyExistentialRestrictionAxiom
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
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyUniversalRestrictionAxiom

  def entityScalarDataPropertyUniversalRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"EntityScalarDataPropertyUniversalRestrictionAxiom("+
        fromIRI(getTermIRI(restrictedEntity))+","+
        fromIRI(getTermIRI(scalarProperty))+","+
        fromIRI(getTermIRI(range))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

  final def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   range: omf#DataRange)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyUniversalRestrictionAxiom
  = for {
    uuid <- entityScalarDataPropertyUniversalRestrictionAxiomUUID(graph, restrictedEntity, scalarProperty, range)
    ax <- addEntityScalarDataPropertyUniversalRestrictionAxiom(graph, uuid, restrictedEntity, scalarProperty, range)
  } yield ax

  protected def addEntityScalarDataPropertyParticularRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   literalValue: LexicalValue)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyParticularRestrictionAxiom

  def entityScalarDataPropertyParticularRestrictionAxiomUUID
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"EntityScalarDataPropertyParticularRestrictionAxiom("+
        fromIRI(getTermIRI(restrictedEntity))+","+
        fromIRI(getTermIRI(scalarProperty))+")")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

  final def addEntityScalarDataPropertyParticularRestrictionAxiom
  (graph: omf#MutableTerminologyBox,
   restrictedEntity: omf#Entity,
   scalarProperty: omf#EntityScalarDataProperty,
   literalValue: LexicalValue)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#EntityScalarDataPropertyParticularRestrictionAxiom
  = for {
    uuid <- entityScalarDataPropertyParticularRestrictionAxiomUUID(graph, restrictedEntity, scalarProperty)
    ax <- addEntityScalarDataPropertyParticularRestrictionAxiom(graph, uuid, restrictedEntity, scalarProperty, literalValue)
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
  : Set[java.lang.Throwable] \/ omf#TerminologyExtensionAxiom

  def terminologyExtensionUUID
  (extendingG: omf#MutableTerminologyBox,
   extendedG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminology(extendingG).iri,
      s"TerminologyExtensionAxiom(${fromIRI(fromTerminology(extendedG).iri)})")
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
  final def addTerminologyExtension
  (extendingG: omf#MutableTerminologyBox,
   extendedG: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyExtensionAxiom
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
   nestedTerminology: omf#MutableTerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyNestingAxiom

  def terminologyNestingAxiomUUID
  (nestingContext: omf#Concept,
   nestedGraph: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(
      fromTerminology(nestedGraph).iri,
      s"TerminologyNestingAxiom(${fromConcept(nestingContext).name})")
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
  final def addNestedTerminology
  (nestingGraph: omf#TerminologyBox,
   nestingContext: omf#Concept,
   nestedGraph: omf#MutableTerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#TerminologyNestingAxiom
  = for {
    uuid <- terminologyNestingAxiomUUID(nestingContext, nestedGraph)
    ax <- addNestedTerminology(uuid, nestingGraph, nestingContext, nestedGraph)
  } yield ax

  /**
    * Add to a terminology graph a new OMF ConceptDesignationTerminologyAxiom.
    *
    * @see https://jpl-imce.github.io/jpl.omf.schema.tables/latest/api/index.html#gov.nasa.jpl.imce.omf.schema.tables.ConceptDesignationTerminologyAxiom
    *
    * @param graph                       The mutable terminology graph in which to assert the axiom
    * @param entityConceptDesignation    The model entity concept whose complete complete designation is specified
    * @param designationTerminologyGraph The terminology graph specifying the complete designation
    *                                    for the structural contents of the model entity concept
    * @param store                       OMF storage provider
    * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
    */
  protected def addEntityConceptDesignationTerminologyAxiom
  (graph: omf#MutableTerminologyBox,
   uuid: UUID,
   designatedConcept: omf#Concept,
   designatedTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ConceptDesignationTerminologyAxiom

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
  : Set[java.lang.Throwable] \/ omf#ConceptDesignationTerminologyAxiom
  = for {
    iri <- withFragment(
      fromTerminology(graph).iri,
      s"ConceptDesignationTerminologyAxiom("+
        fromConcept(entityConceptDesignation).name+","+
        fromIRI(fromTerminology(designationTerminologyGraph).iri)+
        ")")
    ax <- addEntityConceptDesignationTerminologyAxiom(
      graph,
      generateUUID(fromIRI(iri)),
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
  : Set[java.lang.Throwable] \/ omf#BundledTerminologyAxiom

  def bundledTerminologyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   bundledTerminology: omf#TerminologyBox)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminology(terminologyBundle).iri,
      s"BundledTerminologyAxiom(${fromIRI(fromTerminology(bundledTerminology).iri)})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  : Set[java.lang.Throwable] \/ omf#BundledTerminologyAxiom
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
  : Set[java.lang.Throwable] \/ omf#RootConceptTaxonomyAxiom

  def rootConceptTaxonomyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   root: omf#Concept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminology(terminologyBundle).iri,
      s"RootConceptTaxonomyAxiom(${getTermIRI(root)})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  : Set[java.lang.Throwable] \/ omf#RootConceptTaxonomyAxiom
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
   disjointTerminologyParent: omf#ConceptTreeDisjunction)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#AnonymousConceptTaxonomyAxiom

  def anonymousConceptTaxonomyAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminology(terminologyBundle).iri,
      s"AnonymousConceptTaxonomyAxiom(${getConceptTreeDisjunctionUUID(disjointTerminologyParent)})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
   disjointTerminologyParent: omf#ConceptTreeDisjunction)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#AnonymousConceptTaxonomyAxiom
  = for {
    uuid <- anonymousConceptTaxonomyAxiomUUID(terminologyBundle, disjointTerminologyParent)
    ax <- addAnonymousConceptTaxonomyAxiom(uuid, terminologyBundle, disjointTerminologyParent)
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
  : Set[java.lang.Throwable] \/ omf#SpecificDisjointConceptAxiom

  def specificDisjointConceptAxiomUUID
  (terminologyBundle: omf#MutableBundle,
   disjointTerminologyParent: omf#ConceptTreeDisjunction,
   disjointLeaf: omf#Concept)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ UUID
  = for {
    iri <- withFragment(fromTerminology(terminologyBundle).iri,
      s"SpecificDisjointConceptAxiom(${getConceptTreeDisjunctionUUID(disjointTerminologyParent)},${getTermIRI(disjointLeaf)})")
    uuid = generateUUID(fromIRI(iri))
  } yield uuid

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
  : Set[java.lang.Throwable] \/ omf#SpecificDisjointConceptAxiom
  = for {
    uuid <- specificDisjointConceptAxiomUUID(terminologyBundle, disjointTerminologyParent, disjointLeaf)
    ax <- addSpecificDisjointConceptAxiom(uuid, terminologyBundle, disjointTerminologyParent, disjointLeaf)
  } yield ax

}

trait ImmutableInstanceGraphOps[omf <: OMF] {

  def getInstanceGraphIRI
  (graph: omf#ModelInstanceGraph)
  : omf#IRI

  // instance object

  def fromInstanceObject
  (o: omf#ModelInstanceObject)
  : (omf#IRI, omf#Concept)

  // instance relation

  def fromInstanceRelation
  (r: omf#ModelInstanceRelation)
  : (omf#IRI, omf#ReifiedRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance)

  // data literal

  def fromDataLiteral
  (dl: omf#ModelInstanceDataLiteral)
  : (String, omf#Scalar)

  // data structure

  def fromDataStructure
  (ds: omf#ModelInstanceDataStructure)
  : (omf#IRI, omf#Structure)

  // data relationship from entity to scalar

  def fromInstanceDataRelationshipFromEntityToScalar
  (e2sc: omf#ModelInstanceDataRelationshipFromEntityToScalar)
  : (omf#ModelEntityInstance,
    omf#EntityScalarDataProperty,
    omf#ModelInstanceDataLiteral)

  // data relationship from entity to structure

  def fromInstanceDataRelationshipFromEntityToStructure
  (e2sc: omf#ModelInstanceDataRelationshipFromEntityToStructure)
  : (omf#ModelEntityInstance,
    omf#EntityStructuredDataProperty,
    omf#ModelInstanceDataStructure)

  // data relationship from structure to scalar

  def fromInstanceDataRelationshipFromStructureToScalar
  (e2sc: omf#ModelInstanceDataRelationshipFromStructureToScalar)
  : (omf#ModelInstanceDataStructure,
    omf#ScalarDataProperty,
    omf#ModelInstanceDataLiteral)

  // data relationship from structure to structure

  def fromInstanceDataRelationshipFromStructureToStructure
  (e2sc: omf#ModelInstanceDataRelationshipFromStructureToStructure)
  : (omf#ModelInstanceDataStructure,
    omf#StructuredDataProperty,
    omf#ModelInstanceDataStructure)

}

trait MutableInstanceGraphOps[omf <: OMF]
  extends ImmutableInstanceGraphOps[omf] {

  // instance object

  def addInstanceObject
  (graph: omf#MutableModelInstanceGraph,
   conceptType: omf#Concept,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceObject

  // instance relation

  def addInstanceRelation
  (graph: omf#MutableModelInstanceGraph,
   relationshipType: omf#ReifiedRelationship,
   source: omf#ModelEntityInstance,
   target: omf#ModelEntityInstance,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceRelation

  // data literal

  def addDataLiteral
  (graph: omf#MutableModelInstanceGraph,
   datatype: omf#Scalar,
   lexicalForm: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataLiteral

  // data structure

  def addDataStructure
  (graph: omf#MutableModelInstanceGraph,
   datatype: omf#Structure,
   fragment: String)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataStructure

  // data relationship from entity to scalar

  def addInstanceDataRelationshipFromEntityToScalar
  (graph: omf#MutableModelInstanceGraph,
   ei: omf#ModelEntityInstance,
   e2sc: omf#EntityScalarDataProperty,
   value: omf#ModelInstanceDataLiteral)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToScalar

  // data relationship from entity to structure

  def addInstanceDataRelationshipFromEntityToStructure
  (graph: omf#MutableModelInstanceGraph,
   ei: omf#ModelEntityInstance,
   e2st: omf#EntityStructuredDataProperty,
   value: omf#ModelInstanceDataStructure)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToStructure

  // data relationship from structure to scalar

  def addInstanceDataRelationshipFromStructureToScalar
  (graph: omf#MutableModelInstanceGraph,
   di: omf#ModelInstanceDataStructure,
   e2sc: omf#ScalarDataProperty,
   value: omf#ModelInstanceDataLiteral)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToScalar

  // data relationship from structure to structure

  def addInstanceDataRelationshipFromStructureToStructure
  (graph: omf#MutableModelInstanceGraph,
   di: omf#ModelInstanceDataStructure,
   e2st: omf#StructuredDataProperty,
   value: omf#ModelInstanceDataStructure)
  (implicit store: omf#Store)
  : Set[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToStructure

}

trait OMFOps[omf <: OMF]
  extends IRIOps[omf]
    with MutableTerminologyGraphOps[omf]
    with MutableInstanceGraphOps[omf]
    with OMFStoreOps[omf]