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

import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.{Boolean,Option,Unit}
import scala.Predef.String
import scala.collection.immutable.{Iterable,Map,Set}
import scala.language.postfixOps
import scalaz._

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object OMFOps {

  def apply[omf <: OMF]( implicit ops: OMFOps[omf] )
  : OMFOps[omf] = ops

  /**
   * @todo a stream-based closure method
   *
   * @param x initial object of type U
   * @param relation a function from U to V
   * @tparam U
   * @tparam V
   * @return the closure, f(x)+, i.e., f(x), f(f(x)), f(f(f(x))), ...
   */
  def closure[U, V <: U]
  ( x: U, relation: U => Iterable[V] )
  : Set[V] = {

    case class RelationClosureVisitor(
      result: scala.collection.mutable.Set[V],
      visit: scala.collection.mutable.Buffer[V],
      visited: scala.collection.mutable.Set[V] )

    val visitor = RelationClosureVisitor(
      scala.collection.mutable.Set[V](),
      relation( x ).toBuffer,
      scala.collection.mutable.Set[V]() )

    while ( visitor.visit.nonEmpty ) {
      val y = visitor.visit.remove( 0 )
      visitor.visited += y
      visitor.result += y
      relation( y ) foreach ( yi => {
        visitor.result += yi
        if ( !visitor.visited.contains( yi ) ) { visitor.visit += yi }
      } )
    }
    visitor.result.toSet

  }

}

trait IRIOps[omf <: OMF] {

  // IRI

  def makeIRI( s: String )
  : NonEmptyList[java.lang.Throwable] \/ omf#IRI

  def withFragment( iri: omf#IRI, fragment: String )
  : NonEmptyList[java.lang.Throwable] \/ omf#IRI

  /**
   * Split the IRI in two components: the IRI wihtout the fragment, the IRI fragment
   */
  def splitIRI( iri: omf#IRI )
  : ( omf#IRI, Option[String] )

  /**
   * If the IRI has a fragment, returns "n:f" where "n" is the last segment of the IRI and "f" is the fragment of the IRI
   */
  def toAbbreviatedName( iri: omf#IRI, lowercaseFragmentInitial: Boolean )
  : Option[String] 
  

  def fromIRI( iri: omf#IRI )
  : String

  /**
   * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return true if <host> == imce.jpl.nasa.gov and <path> starts with /backbone
   */
  def isBackboneIRI( iri: omf#IRI )
  : Boolean

  /**
   * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host'><port><path'><query><fragment>
   * where:
   * <host'> = imce.jpl.nasa.gov
   * <path'> = /backbone/<host><path>
   */
  def toBackboneIRI( iri: omf#IRI )
  : omf#IRI

  /**
   * Produces the canonical "has...Source" IRI from the IRI of an entity relationship or data relationship to a structure
   * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
   * where:
   * <fragment'> = has<fragment>Source
   */
  def toSourceIRI( iri: omf#IRI )
  : omf#IRI

  /**
   * Produces the canonical "has...Target" IRI for the IRI of an entity relationship or data relationship to a structure
   * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
   * where:
   * <fragment'> = has<fragment>Target
   */
  def toTargetIRI( iri: omf#IRI )
  : omf#IRI

}

trait OMFStoreOps[omf <: OMF] {

  def loadTerminologyGraph
  ( iri: omf#IRI )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ (omf#ImmutableModelTerminologyGraph, omf#Mutable2IMutableTerminologyMap)

  def fromTerminologyGraph
  ( graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : TerminologyGraphSignature[omf]

  /**
   * Assigns a designation terminology graph as the closed-world structural description of a model entity concept
   * @param graph The mutable terminology graph in which to assert the axiom
   * @param entityConceptDesignation The model entity concept whose complete complete designation is specified
   * @param designationTerminologyGraph The terminology graph specifying the complete designation
   *                                    for the structural contents of the model entity concept
   * @param store
   * @return The EntityConceptToplevelDesignationTerminologyGraphAxiom created
   */
  def addEntityConceptDesignationTerminologyGraphAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    entityConceptDesignation: omf#ModelEntityConcept,
    designationTerminologyGraph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityConceptDesignationTerminologyGraphAxiom

  def getNestingGraph
  ( graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Option[omf#ModelTerminologyGraph]

  def getNestedGraphs
  ( graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Iterable[omf#ModelTerminologyGraph]

  def addNestedTerminologyGraph
  ( parentG: omf#MutableModelTerminologyGraph,
    nestedG: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#TerminologyGraphDirectNestingAxiom

  def getDirectlyExtendingGraphsOfExtendedParentGraph
  (extendedParentG: omf#ModelTerminologyGraph)
  : Iterable[omf#TerminologyGraphDirectExtensionAxiom]

  def getDirectlyExtendedGraphsOfExtendingChildGraph
  (extendingChildG: omf#ModelTerminologyGraph)
  : Iterable[omf#TerminologyGraphDirectExtensionAxiom]

  def addTerminologyGraphExtension
  ( extendingG: omf#MutableModelTerminologyGraph,
    extendedG: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#TerminologyGraphDirectExtensionAxiom

  /**
   * Create a mutable terminology graph partially identified by an IRI and a kind.
   *
   * The complete identity of a graph includes the IRI, kind and imported/extended graphs.
   * For a mutable terminology graph, imported/extended graphs must be specified
   * via `addTerminologyGraphExtension`
   *
   * @param iri the identity of the new mutable terminology graph
   * @param kind the kind of the new mutable terminology graph
   */
  def makeTerminologyGraph
  ( iri: omf#IRI,
    kind: TerminologyKind )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#MutableModelTerminologyGraph

  def saveTerminologyGraph
  ( g: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  def saveTerminologyGraph
  ( g: omf#ModelTerminologyGraph,
    os: java.io.OutputStream )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  /**
   * Converts a mutable tbox graph into an equivalent immutable tbox graph such that
   * dependencies on mutable tbox graphs are also converted into equivalent immutable tbox graphs.
   *
   * @param g a mutable tbox
   * @param store
   * @return a map of all the mutable tboxes (incl. g) that have been converted to immutable tboxes
   */
  def asImmutableTerminologyGraph
  ( g: omf#MutableModelTerminologyGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ (omf#ImmutableModelTerminologyGraph, Map[omf#MutableModelTerminologyGraph, omf#ImmutableModelTerminologyGraph])

  def isEntityDefinitionAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    (s.aspects.toSet contains t ) ||
      ( s.concepts.toSet contains t ) || (
      s.reifiedRelationships.toSet contains t )
  }

  def isEntityDefinitionImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityDefinitionAssertedInTerminologyGraph( t, _ ) )

  def isEntityUnreifiedRelationshipAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.reifiedRelationships.toSet contains t
  }

  def isEntityUnreifiedRelationshipImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityUnreifiedRelationshipAssertedInTerminologyGraph( t, _ ) )

  def isScalarDataTypeAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.scalarDataTypes.toSet contains t
  }

  def isScalarDataTypeImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isScalarDataTypeAssertedInTerminologyGraph( t, _ ) )

  def isStructuredDataTypeAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.structuredDataTypes.toSet contains t
  }

  def isStructuredDataTypeImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isStructuredDataTypeAssertedInTerminologyGraph( t, _ ) )

  def isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.entity2scalarDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromEntityToScalarImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph( t, _ ) )

  def isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.entity2structureDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromEntityToStructureImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph( t, _ ) )

  def isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.structure2scalarDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromStructureToScalarImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph( t, _ ) )

  def isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit store: omf#Store )
  : Boolean = {
    val s = fromTerminologyGraph( graph )
    s.structure2structureDataRelationships.toSet contains t
  }

  def isEntityDataRelationshipFromStructureToStructureImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph( t, _ ) )

  def isTypeTermAssertedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    isEntityDefinitionAssertedInTerminologyGraph( t, graph ) ||
      isEntityUnreifiedRelationshipAssertedInTerminologyGraph( t, graph ) ||
      isScalarDataTypeAssertedInTerminologyGraph( t, graph ) ||
      isStructuredDataTypeAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph( t, graph )

  def isTypeTermImportedInTerminologyGraph
  ( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Boolean =
    terminologyGraphImportClosure[omf, omf#ModelTerminologyGraph](graph).
      exists( isTypeTermAssertedInTerminologyGraph( t, _ ) )

  def loadInstanceGraph
  ( iri: omf#IRI )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ImmutableModelInstanceGraph

  def fromInstanceGraph
  ( graph: omf#ModelInstanceGraph )
  : ( omf#IRI,
    Iterable[omf#ImmutableModelTerminologyGraph],
    Iterable[omf#ModelInstanceGraph],
    Iterable[omf#ModelInstanceObject],
    Iterable[omf#ModelInstanceRelation],
    Iterable[omf#ModelInstanceDataLiteral],
    Iterable[omf#ModelInstanceDataStructure],
    Iterable[omf#ModelInstanceDataRelationshipFromEntityToScalar],
    Iterable[omf#ModelInstanceDataRelationshipFromEntityToStructure],
    Iterable[omf#ModelInstanceDataRelationshipFromStructureToScalar],
    Iterable[omf#ModelInstanceDataRelationshipFromStructureToStructure] )

  def makeInstanceGraph
  ( iri: omf#IRI,
    instantiatedTGraphs: Iterable[omf#ImmutableModelTerminologyGraph],
    extendedIGraphs: Iterable[omf#ImmutableModelInstanceGraph] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#MutableModelInstanceGraph

  def asImmutableInstanceGraph
  ( g: omf#MutableModelInstanceGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ImmutableModelInstanceGraph

  def saveInstanceGraph
  ( g: omf#ModelInstanceGraph )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  /**
   * @since 0.10.2
   */
  def saveInstanceGraph
  ( g: omf#ModelInstanceGraph, os: java.io.OutputStream )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

}

trait ImmutableTerminologyGraphOps[omf <: OMF] {

  def getTerminologyGraphIRI
  ( graph: omf#ModelTerminologyGraph )
  : omf#IRI

  def getTerminologyGraphShortName
  ( graph: omf#ModelTerminologyGraph )
  : Option[String]

  def getTerminologyGraphUUID
  ( graph: omf#ModelTerminologyGraph )
  : Option[String]

  def getTerminologyGraphKind
  ( graph: omf#ModelTerminologyGraph )
  : TerminologyKind


  def lookupTypeTerm
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelTypeTerm]

  def lookupEntityDefinition
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelEntityDefinition]

  def lookupEntityAspect
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelEntityAspect]

  def lookupEntityConcept
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelEntityConcept]

  def lookupEntityReifiedRelationship
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelEntityReifiedRelationship]

  def lookupEntityUnreifiedRelationship
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelEntityUnreifiedRelationship]

  def lookupScalarDataType
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelScalarDataType]

  def lookupStructuredDataType
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelStructuredDataType]

  def lookupEntityDataRelationshipFromEntityToScalar
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelDataRelationshipFromEntityToScalar]

  def lookupEntityDataRelationshipFromEntityToStructure
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelDataRelationshipFromEntityToStructure]

  def lookupEntityDataRelationshipFromStructureToScalar
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelDataRelationshipFromStructureToScalar]

  def lookupEntityDataRelationshipFromStructureToStructure
  ( graph: omf#ModelTerminologyGraph, iri: omf#IRI, recursively: Boolean )
  ( implicit store: omf#Store )
  : Option[omf#ModelDataRelationshipFromStructureToStructure]

  def getTermAxioms
  ( graph: omf#ModelTerminologyGraph )
  : ( omf#IRI, Iterable[omf#ModelTermAxiom] )

  def getTypeTerms
  ( graph: omf#ModelTerminologyGraph )
  : ( omf#IRI, Iterable[omf#ModelTypeTerm] )

  def foldTerm[T]
  ( t: omf#ModelTypeTerm )
  ( funEntityConcept: omf#ModelEntityConcept => T,
    funEntityReifiedRelationship: omf#ModelEntityReifiedRelationship => T,
    funEntityUnreifiedRelationship: omf#ModelEntityUnreifiedRelationship => T,
    funScalarDataType: omf#ModelScalarDataType => T,
    funStructuredDataType: omf#ModelStructuredDataType => T,
    funDataRelationshipFromEntityToScalar: omf#ModelDataRelationshipFromEntityToScalar => T,
    funDataRelationshipFromEntityToStructure: omf#ModelDataRelationshipFromEntityToStructure => T,
    funDataRelationshipFromStructureToScalar: omf#ModelDataRelationshipFromStructureToScalar => T,
    funDataRelationshipFromStructureToStructure: omf#ModelDataRelationshipFromStructureToStructure => T )
  : T

  def getTermShortName
  ( graph: omf#ModelTerminologyGraph,
    term: omf#ModelTypeTerm )
  : Option[String]

  def getTermShortUUID
  ( graph: omf#ModelTerminologyGraph,
    term: omf#ModelTypeTerm )
  : Option[String]

  def fromTerm
  ( t: omf#ModelTypeTerm )
  : omf#IRI =
    foldTerm[omf#IRI]( t )(
    ( ec: omf#ModelEntityConcept ) =>
      fromEntityConcept( ec ).iri,
    ( er: omf#ModelEntityReifiedRelationship ) =>
      fromEntityReifiedRelationship( er ).iri,
    ( ur: omf#ModelEntityUnreifiedRelationship ) =>
      fromEntityUnreifiedRelationship( ur ).iri,
    ( sc: omf#ModelScalarDataType ) =>
      fromScalarDataType( sc ),
    ( sd: omf#ModelStructuredDataType ) =>
      fromStructuredDataType( sd ),
    ( esc: omf#ModelDataRelationshipFromEntityToScalar ) =>
      fromDataRelationshipFromEntityToScalar( esc )._1,
    ( est: omf#ModelDataRelationshipFromEntityToStructure ) =>
      fromDataRelationshipFromEntityToStructure( est )._1,
    ( ssc: omf#ModelDataRelationshipFromStructureToScalar ) =>
      fromDataRelationshipFromStructureToScalar( ssc )._1,
    ( sst: omf#ModelDataRelationshipFromStructureToStructure ) =>
      fromDataRelationshipFromStructureToStructure( sst )._1 )

  // entity aspect

  def fromEntityAspect( t: omf#ModelEntityAspect )
  : omf#IRI

  // entity definition

  def fromEntityDefinition( e: omf#ModelEntityDefinition )
  : omf#IRI

  // entity concept

  /**
   * @param c A concept
   * @return A tuple consisting of:
   * - the IRI of the concept
   * - if any, the IRI of the graph corresponding to the concept
   * - a boolean flag indicating whether this is an abstract concept or not
   * @since 0.10.3
   */
  def fromEntityConcept
  ( c: omf#ModelEntityConcept )
  : EntityConceptSignature[omf]

  def equivalentEntityConcepts
  ( c1: Iterable[omf#ModelEntityConcept], c2: Iterable[omf#ModelEntityConcept] )
  : Boolean = {
    val iris1 = c1.map( fromEntityConcept ).toSet
    val iris2 = c2.map( fromEntityConcept ).toSet
    val d = iris1.diff( iris2 )
    d.isEmpty
  }

  // entity relationship


  /**
   * @param r, a relationship
   * @return a tuple consisting of:
   * - the IRI of the relationship
   * - the IRI of the graph corresponding to the relationship, if any
   * - the source entity of the relationship
   * - the target entity of the relationship
   * - the characteristics of the relationship
   * - a flag indicating whether the relationship is abstract or not.
   */
  def fromEntityReifiedRelationship
  ( r: omf#ModelEntityReifiedRelationship )
  : EntityReifiedRelationshipSignature[omf]

  def fromEntityUnreifiedRelationship
  ( r: omf#ModelEntityUnreifiedRelationship )
  : EntityUnreifiedRelationshipSignature[omf]

  /**
   * Compares the relationships in terms of their sources, target & characteristics
   * Does not compare the graphs corresponding to each relationship, if any	. 
   * @since 0.10.3	
   */
  def equivalentEntityReifiedRelationships
  ( r1: Iterable[omf#ModelEntityReifiedRelationship],
    r2: Iterable[omf#ModelEntityReifiedRelationship] )
  : Boolean = {
    val left = r1.map { r => 
      val s = fromEntityReifiedRelationship( r )
      ( s.iri,
        fromEntityDefinition( s.source ),
        fromEntityDefinition( s.target ),
        relationshipCharacteristicsSummary( s.characteristics ) )
    }
    .toSet
    val right = r2.map { r =>
      val s = fromEntityReifiedRelationship( r )
      ( s.iri,
        fromEntityDefinition( s.source ),
        fromEntityDefinition( s.target ),
        relationshipCharacteristicsSummary( s.characteristics ) )
    }
    .toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // datatype definition

  def fromDataTypeDefinition
  ( dt: omf#ModelDataTypeDefinition )
  : omf#IRI

  // scalar datatype

  def fromScalarDataType
  ( dt: omf#ModelScalarDataType )
  : omf#IRI

  def equivalentScalarDataTypes
  ( dt1: Iterable[omf#ModelScalarDataType],
    dt2: Iterable[omf#ModelScalarDataType] )
  : Boolean = {
    val left = dt1.map( fromScalarDataType ).toSet
    val right = dt2.map( fromScalarDataType ).toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // structured datatype

  def fromStructuredDataType
  ( dt: omf#ModelStructuredDataType )
  : omf#IRI

  def equivalentStructuredDataTypes
  ( dt1: Iterable[omf#ModelStructuredDataType],
    dt2: Iterable[omf#ModelStructuredDataType] )
  : Boolean = {
    val left = dt1.map( fromStructuredDataType ).toSet
    val right = dt2.map( fromStructuredDataType ).toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // data relationship from entity to scalar

  def fromDataRelationshipFromEntityToScalar
  ( esc: omf#ModelDataRelationshipFromEntityToScalar )
  : ( omf#IRI, omf#ModelEntityDefinition, omf#ModelScalarDataType )

  // data relationship from entity to structure

  def fromDataRelationshipFromEntityToStructure
  ( est: omf#ModelDataRelationshipFromEntityToStructure )
  : ( omf#IRI, omf#ModelEntityDefinition, omf#ModelStructuredDataType )

  // data relationship from structure to scalar

  def fromDataRelationshipFromStructureToScalar
  ( esc: omf#ModelDataRelationshipFromStructureToScalar )
  : ( omf#IRI, omf#ModelStructuredDataType, omf#ModelScalarDataType )

  // data relationship from structure to structure

  def fromDataRelationshipFromStructureToStructure
  ( est: omf#ModelDataRelationshipFromStructureToStructure )
  : ( omf#IRI, omf#ModelStructuredDataType, omf#ModelStructuredDataType )

  // model term axioms

  def foldTermAxiom[T]
  ( t: omf#ModelTermAxiom )
  ( funEntityDefinitionAspectSubClassAxiom: omf#EntityDefinitionAspectSubClassAxiom => T,
    funEntityConceptDesignationTerminologyGraphAxiom: omf#EntityConceptDesignationTerminologyGraphAxiom => T,
    funEntityConceptSubClassAxiom: omf#EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: omf#EntityConceptRestrictionAxiom => T,
    funEntityReifiedRelationshipSubClassAxiom: omf#EntityReifiedRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestrictionAxiom: omf#ScalarDataTypeFacetRestrictionAxiom => T )
  : T

  // entity definition aspect subclass axiom

  def fromEntityDefinitionAspectSubClassAxiom
  ( ax: omf#EntityDefinitionAspectSubClassAxiom )
  : ( omf#ModelEntityDefinition, omf#ModelEntityAspect )

  // entity concept designation terminology graph axiom

  def fromEntityConceptDesignationTerminologyGraphAxiom
  ( ax: omf#EntityConceptDesignationTerminologyGraphAxiom )
  : ( omf#ModelEntityConcept, omf#ModelTerminologyGraph )


  // entity concept subclass axiom

  def fromEntityConceptSubClassAxiom
  ( ax: omf#EntityConceptSubClassAxiom )
  : ( omf#ModelEntityConcept, omf#ModelEntityConcept )

  // entity concept restriction axiom

  def fromEntityConceptRestrictionAxiom
  ( ax: omf#EntityConceptRestrictionAxiom )
  : ( omf#ModelEntityConcept, omf#ModelEntityReifiedRelationship, omf#ModelEntityDefinition )

  // entity relationship subclass axiom

  def fromEntityReifiedRelationshipSubClassAxiom
  ( ax: omf#EntityReifiedRelationshipSubClassAxiom )
  : ( omf#ModelEntityReifiedRelationship, omf#ModelEntityReifiedRelationship )

  // scalar datatype facet restriction axiom

  def fromScalarDataTypeFacetRestrictionAxiom
  ( ax: omf#ScalarDataTypeFacetRestrictionAxiom )
  : ( omf#ModelScalarDataType, omf#ModelScalarDataType, Iterable[ConstrainingFacet] )

}

trait MutableTerminologyGraphOps[omf <: OMF] extends ImmutableTerminologyGraphOps[omf] {

  def setTerminologyGraphShortName
  ( graph: omf#MutableModelTerminologyGraph,
    name: Option[String] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  def setTerminologyGraphUUID
  ( graph: omf#MutableModelTerminologyGraph,
    uuid: Option[String] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  def setTermShortName
  ( g: omf#MutableModelTerminologyGraph,
    term: omf#ModelTypeTerm,
    name: Option[String] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  def setTermUUID
  ( g: omf#MutableModelTerminologyGraph,
    term: omf#ModelTypeTerm,
    uuid: Option[String] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ Unit

  /**
   * Add to a terminology graph a new ModelEntityAspect
   *
   * @param graph: a terminology graph
   * @param aspectName: the name of a new entity aspect
   */
  def addEntityAspect
  ( graph: omf#MutableModelTerminologyGraph,
    aspectName: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelEntityAspect

  /**
   * Add to a terminology graph a new ModelEntityConcept
   *
   * @param graph: a terminology graph
   * @param conceptName: the name of a new entity concept
   * @param isAbstract: boolean flag
   */
  def addEntityConcept
  ( graph: omf#MutableModelTerminologyGraph,
    conceptName: String,
    isAbstract: Boolean )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelEntityConcept

  /**
   * Add to a terminology graph a new ModelEntityReifiedRelationship
   *
   * @param graph: a terminology graph
   * @param source: an existing entity definition that will be
   *              the source of the new entity relationship
   * @param target: an existing entity definition that will be
   *              the target of the new entity relationship
   * @param characteristics: the characteristics of the new entity relationship
   * @param reifiedRelationshipName: the name of the new entity relationship
   *                               from the perspective of a reified concept-like entity
   * @param unreifiedRelationshipName: the name of the entity relationship from the perspective
   *                                 of a directed property from the source to the target
   * @param unreifiedInverseRelationshipName: if applicable, the name of the entity relationship from
   *                                        the perspective of a directed inverse property
   *                                        from the target to the source
   * @param isAbstract: boolean flag
   */
  def addEntityReifiedRelationship
  ( graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String],
    isAbstract: Boolean )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelEntityReifiedRelationship

  def addScalarDataType
  ( graph: omf#MutableModelTerminologyGraph,
    fragment: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelScalarDataType

  def addStructuredDataType
  ( graph: omf#MutableModelTerminologyGraph,
    fragment: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelStructuredDataType

  def addDataRelationshipFromEntityToScalar
  ( graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelScalarDataType,
    dataRelationshipName: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToScalar

  def addDataRelationshipFromEntityToStructure
  ( graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelStructuredDataType,
    dataRelationshipName: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelDataRelationshipFromEntityToStructure

  def addDataRelationshipFromStructureToScalar
  ( graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelStructuredDataType,
    target: omf#ModelScalarDataType,
    dataRelationshipName: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToScalar

  def addDataRelationshipFromStructureToStructure
  ( graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelStructuredDataType,
    target: omf#ModelStructuredDataType,
    dataRelationshipName: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelDataRelationshipFromStructureToStructure

  // model term axioms

  def addEntityDefinitionAspectSubClassAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityDefinition,
    sup: omf#ModelEntityAspect )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityDefinitionAspectSubClassAxiom

  def addEntityConceptSubClassAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    sup: omf#ModelEntityConcept )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityConceptSubClassAxiom

  def addEntityConceptUniversalRestrictionAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    rel: omf#ModelEntityReifiedRelationship,
    range: omf#ModelEntityDefinition )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityConceptUniversalRestrictionAxiom

  def addEntityConceptExistentialRestrictionAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    rel: omf#ModelEntityReifiedRelationship,
    range: omf#ModelEntityDefinition )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityConceptExistentialRestrictionAxiom

  def addEntityReifiedRelationshipSubClassAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityReifiedRelationship,
    sup: omf#ModelEntityReifiedRelationship )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#EntityReifiedRelationshipSubClassAxiom

  def addScalarDataTypeFacetRestrictionAxiom
  ( graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelScalarDataType,
    sup: omf#ModelScalarDataType,
    facets: Iterable[ConstrainingFacet] )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ScalarDataTypeFacetRestrictionAxiom

}

trait ImmutableInstanceGraphOps[omf <: OMF] {

  def getInstanceGraphIRI
  ( graph: omf#ModelInstanceGraph )
  : omf#IRI

  // instance object

  def fromInstanceObject
  ( o: omf#ModelInstanceObject )
  : ( omf#IRI, omf#ModelEntityConcept )

  // instance relation

  def fromInstanceRelation
  ( r: omf#ModelInstanceRelation )
  : ( omf#IRI, omf#ModelEntityReifiedRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance )

  // data literal

  def fromDataLiteral
  ( dl: omf#ModelInstanceDataLiteral )
  : ( String, omf#ModelScalarDataType )

  // data structure

  def fromDataStructure
  ( ds: omf#ModelInstanceDataStructure )
  : ( omf#IRI, omf#ModelStructuredDataType )

  // data relationship from entity to scalar

  def fromInstanceDataRelationshipFromEntityToScalar
  ( e2sc: omf#ModelInstanceDataRelationshipFromEntityToScalar )
  : ( omf#ModelEntityInstance,
    omf#ModelDataRelationshipFromEntityToScalar,
    omf#ModelInstanceDataLiteral )

  // data relationship from entity to structure

  def fromInstanceDataRelationshipFromEntityToStructure
  ( e2sc: omf#ModelInstanceDataRelationshipFromEntityToStructure )
  : ( omf#ModelEntityInstance,
    omf#ModelDataRelationshipFromEntityToStructure,
    omf#ModelInstanceDataStructure )

  // data relationship from structure to scalar

  def fromInstanceDataRelationshipFromStructureToScalar
  ( e2sc: omf#ModelInstanceDataRelationshipFromStructureToScalar )
  : ( omf#ModelInstanceDataStructure,
    omf#ModelDataRelationshipFromStructureToScalar,
    omf#ModelInstanceDataLiteral )

  // data relationship from structure to structure

  def fromInstanceDataRelationshipFromStructureToStructure
  ( e2sc: omf#ModelInstanceDataRelationshipFromStructureToStructure )
  : ( omf#ModelInstanceDataStructure,
    omf#ModelDataRelationshipFromStructureToStructure,
    omf#ModelInstanceDataStructure )

}

trait MutableInstanceGraphOps[omf <: OMF]
  extends ImmutableInstanceGraphOps[omf] {

  // instance object

  def addInstanceObject
  ( graph: omf#MutableModelInstanceGraph,
    conceptType: omf#ModelEntityConcept,
    fragment: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceObject

  // instance relation

  def addInstanceRelation
  ( graph: omf#MutableModelInstanceGraph,
    relationshipType: omf#ModelEntityReifiedRelationship,
    source: omf#ModelEntityInstance,
    target: omf#ModelEntityInstance,
    fragment: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceRelation

  // data literal

  def addDataLiteral
  ( graph: omf#MutableModelInstanceGraph,
    datatype: omf#ModelScalarDataType,
    lexicalForm: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataLiteral

  // data structure

  def addDataStructure
  ( graph: omf#MutableModelInstanceGraph,
    datatype: omf#ModelStructuredDataType,
    fragment: String )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataStructure

  // data relationship from entity to scalar

  def addInstanceDataRelationshipFromEntityToScalar
  ( graph: omf#MutableModelInstanceGraph,
    ei: omf#ModelEntityInstance,
    e2sc: omf#ModelDataRelationshipFromEntityToScalar,
    value: omf#ModelInstanceDataLiteral )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToScalar

  // data relationship from entity to structure

  def addInstanceDataRelationshipFromEntityToStructure
  ( graph: omf#MutableModelInstanceGraph,
    ei: omf#ModelEntityInstance,
    e2st: omf#ModelDataRelationshipFromEntityToStructure,
    value: omf#ModelInstanceDataStructure )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromEntityToStructure

  // data relationship from structure to scalar

  def addInstanceDataRelationshipFromStructureToScalar
  ( graph: omf#MutableModelInstanceGraph,
    di: omf#ModelInstanceDataStructure,
    e2sc: omf#ModelDataRelationshipFromStructureToScalar,
    value: omf#ModelInstanceDataLiteral )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToScalar

  // data relationship from structure to structure

  def addInstanceDataRelationshipFromStructureToStructure
  ( graph: omf#MutableModelInstanceGraph,
    di: omf#ModelInstanceDataStructure,
    e2st: omf#ModelDataRelationshipFromStructureToStructure,
    value: omf#ModelInstanceDataStructure )
  ( implicit store: omf#Store )
  : NonEmptyList[java.lang.Throwable] \/ omf#ModelInstanceDataRelationshipFromStructureToStructure

}

trait OMFOps[omf <: OMF]
  extends IRIOps[omf]
  with MutableTerminologyGraphOps[omf]
  with MutableInstanceGraphOps[omf]
  with OMFStoreOps[omf]