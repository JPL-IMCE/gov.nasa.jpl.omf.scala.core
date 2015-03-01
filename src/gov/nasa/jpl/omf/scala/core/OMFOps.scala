/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.io.OutputStream

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object OMFOps {

  def apply[omf <: OMF]( implicit ops: OMFOps[omf] ): OMFOps[omf] = ops

  def closure[U, V <: U]( x: U, relation: U => Iterable[V] ): Set[V] = {

    case class RelationClosureVisitor(
      result: scala.collection.mutable.Set[V],
      visit: scala.collection.mutable.Buffer[V],
      visited: scala.collection.mutable.Set[V] )

    val visitor = RelationClosureVisitor( scala.collection.mutable.Set[V](), relation( x ).toBuffer, scala.collection.mutable.Set[V]() )
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

trait IRIOps[omf <: OMFiri] {

  // IRI

  def makeIRI( s: String ): omf#IRI

  def withFragment( iri: omf#IRI, fragment: String ): Try[omf#IRI]

  def splitIRI( iri: omf#IRI ): ( omf#IRI, Option[String] )

  def fromIRI( iri: omf#IRI ): String

  /**
   * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return true if <host> == imce.jpl.nasa.gov and <path> starts with /backbone
   */
  def isBackboneIRI( iri: omf#IRI ): Boolean

  /**
   * @param iri of the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host'><port><path'><query><fragment>
   * where:
   * <host'> = imce.jpl.nasa.gov
   * <path'> = /backbone/<host><path>
   */
  def toBackboneIRI( iri: omf#IRI ): omf#IRI

  /**
   * Produces the canonical "has...Source" IRI from the IRI of an entity relationship or data relationship to a structure
   * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
   * where:
   * <fragment'> = has<fragment>Source
   */
  def toSourceIRI( iri: omf#IRI ): omf#IRI

  /**
   * Produces the canonical "has...Target" IRI for the IRI of an entity relationship or data relationship to a structure
   * @param iri of a reified object property class the form: <scheme><userInfo><host><port><path><query><fragment>
   * @return a new IRI of the form: <scheme><userInfo><host><port><path><query><fragment'>
   * where:
   * <fragment'> = has<fragment>Target
   */
  def toTargetIRI( iri: omf#IRI ): omf#IRI

}

trait ImmutableTerminologyGraphOps[omf <: OMFiri with OMFtbox with OMFstore] {

  def loadTerminologyGraph( iri: omf#IRI )( implicit store: omf#Store ): Try[omf#ImmutableModelTerminologyGraph]

  def getTerminologyGraphIRI( graph: omf#ModelTerminologyGraph ): omf#IRI

  def getTerminologyGraphKind( graph: omf#ModelTerminologyGraph ): TerminologyKind

  /**
   * Query an OMF model terminology graph for its characteristics.
   *
   * @param graph An OMF model terminology graph (mutable or immutable)
   * @return a tuple of the characteristics of the `graph`, which can be grouped in 3 topics:
   * 1) Identity & characteristics
   * - the `graph` IRI
   * - the IRI of the entity corresponding to the graph, if any
   * - the kind of `graph` ( definition vs. designation )
   * - the other graphs extended by this `graph`
   * 2) Definitions
   * - the aspects in `graph`
   * - the concepts in `graph`
   * - the relationships in `graph`
   * - the scalar datatypes in `graph`
   * - the structured datatypes in `graph
   * - the data relationships from an entity to a scalar datatype in `graph`
   * - the data relationships from an entity to a structured datatype in `graph`
   * - the data relationships from an structured datatype to a scalar datatype in `graph`
   * - the data relationships from an structured datatype to a scalar datatype in `graph`
   * 3) Constraints
   * - the axioms asserted in the `graph` about definitions from the closure of graph extension relationships
   * @since 0.10.3
   */
  def fromTerminologyGraph( graph: omf#ModelTerminologyGraph ): ( omf#IRI, Option[omf#IRI], TerminologyKind, Iterable[omf#ModelTerminologyGraph], Iterable[omf#ModelEntityAspect], Iterable[omf#ModelEntityConcept], Iterable[omf#ModelEntityRelationship], Iterable[omf#ModelScalarDataType], Iterable[omf#ModelStructuredDataType], Iterable[omf#ModelDataRelationshipFromEntityToScalar], Iterable[omf#ModelDataRelationshipFromEntityToStructure], Iterable[omf#ModelDataRelationshipFromStructureToScalar], Iterable[omf#ModelDataRelationshipFromStructureToStructure], Iterable[omf#ModelTermAxiom] )

  def isEntityDefinitionAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
    ( _c.toSet contains t ) || ( _r.toSet contains t )
  }

  def isEntityDefinitionImportedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    !isEntityDefinitionAssertedInTerminologyGraph( t, graph ) && {
      val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
      _i.exists( ig => isEntityDefinitionAssertedInTerminologyGraph( t, ig ) || isEntityDefinitionImportedInTerminologyGraph( t, ig ) )
    }
  }

  def isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
    ( _esc.toSet contains t )
  }

  def isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
    ( _est.toSet contains t )
  }

  def isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
    ( _ssc.toSet contains t )
  }

  def isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _, _k, _i, _f, _c, _r, _sc, _st, _esc, _est, _ssc, _sst, _ax ) = fromTerminologyGraph( graph )
    ( _sst.toSet contains t )
  }

  def isTypeTermAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean =
    isEntityDefinitionAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromEntityToScalarAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromEntityToStructureAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromStructureToScalarAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipFromStructureToStructureAssertedInTerminologyGraph( t, graph )

  def lookupTypeTerm( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelTypeTerm]

  def lookupEntityDefinition( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityDefinition]
  def lookupEntityAspect( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityAspect]
  def lookupEntityConcept( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityConcept]
  def lookupEntityRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityRelationship]
  def lookupScalarDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelScalarDataType]
  def lookupStructuredDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelStructuredDataType]
  def lookupEntityDataRelationshipFromEntityToScalar( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelDataRelationshipFromEntityToScalar]
  def lookupEntityDataRelationshipFromEntityToStructure( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelDataRelationshipFromEntityToStructure]
  def lookupEntityDataRelationshipFromStructureToScalar( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelDataRelationshipFromStructureToScalar]
  def lookupEntityDataRelationshipFromStructureToStructure( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelDataRelationshipFromStructureToStructure]

  def getTerms( graph: omf#ModelTerminologyGraph ): ( omf#IRI, Iterable[omf#ModelTypeTerm] )

  def foldTerm[T]( t: omf#ModelTypeTerm )(
    funEntityConcept: omf#ModelEntityConcept => T,
    funEntityRelationship: omf#ModelEntityRelationship => T,
    funScalarDataType: omf#ModelScalarDataType => T,
    funStructuredDataType: omf#ModelStructuredDataType => T,
    funDataRelationshipFromEntityToScalar: omf#ModelDataRelationshipFromEntityToScalar => T,
    funDataRelationshipFromEntityToStructure: omf#ModelDataRelationshipFromEntityToStructure => T,
    funDataRelationshipFromStructureToScalar: omf#ModelDataRelationshipFromStructureToScalar => T,
    funDataRelationshipFromStructureToStructure: omf#ModelDataRelationshipFromStructureToStructure => T ): T

  def fromTerm( t: omf#ModelTypeTerm ): omf#IRI = foldTerm[omf#IRI]( t )(
    ( ec: omf#ModelEntityConcept ) => fromEntityConcept( ec )._1,
    ( er: omf#ModelEntityRelationship ) => fromEntityRelationship( er )._1,
    ( sc: omf#ModelScalarDataType ) => fromScalarDataType( sc ),
    ( sd: omf#ModelStructuredDataType ) => fromStructuredDataType( sd ),
    ( esc: omf#ModelDataRelationshipFromEntityToScalar ) => fromDataRelationshipFromEntityToScalar( esc )._1,
    ( est: omf#ModelDataRelationshipFromEntityToStructure ) => fromDataRelationshipFromEntityToStructure( est )._1,
    ( ssc: omf#ModelDataRelationshipFromStructureToScalar ) => fromDataRelationshipFromStructureToScalar( ssc )._1,
    ( sst: omf#ModelDataRelationshipFromStructureToStructure ) => fromDataRelationshipFromStructureToStructure( sst )._1 )

  // entity aspect

  def fromEntityAspect( t: omf#ModelEntityAspect ): omf#IRI

  // entity definition

  def fromEntityDefinition( e: omf#ModelEntityDefinition ): omf#IRI

  // entity concept

  /**
   * @param c A concept
   * @return A tuple consisting of:
   * - the IRI of the concept
   * - if any, the IRI of the graph corresponding to the concept
   * - a boolean flag indicating whether this is an abstract concept or not
   * @since 0.10.3
   */
  def fromEntityConcept( c: omf#ModelEntityConcept ): ( omf#IRI, Option[omf#IRI], Boolean )

  def equivalentEntityConcepts( c1: Iterable[omf#ModelEntityConcept], c2: Iterable[omf#ModelEntityConcept] ): Boolean = {
    val iris1 = c1.map( fromEntityConcept( _ ) ) toSet
    val iris2 = c2.map( fromEntityConcept( _ ) ) toSet
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
  def fromEntityRelationship( r: omf#ModelEntityRelationship ): ( omf#IRI, Option[omf#IRI], omf#ModelEntityDefinition, omf#ModelEntityDefinition, Iterable[RelationshipCharacteristics], Boolean )

  /**
   * Compares the relationships in terms of their sources, target & characteristics
   * Does not compare the graphs corresponding to each relationship, if any	. 
   * @since 0.10.3	
   */
  def equivalentEntityRelationships( r1: Iterable[omf#ModelEntityRelationship], r2: Iterable[omf#ModelEntityRelationship] ): Boolean = {
    val left = r1.map { r => 
      val ( i, _, s, t, c, a ) = fromEntityRelationship( r )
      ( i, fromEntityDefinition( s ), fromEntityDefinition( t ), relationshipCharacteristicsSummary( c ) )
    } toSet
    val right = r2.map { r => 
      val ( i, _, s, t, c, a ) = fromEntityRelationship( r )
      ( i, fromEntityDefinition( s ), fromEntityDefinition( t ), relationshipCharacteristicsSummary( c ) ) 
    } toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // datatype definition

  def fromDataTypeDefinition( dt: omf#ModelDataTypeDefinition ): omf#IRI

  // scalar datatype

  def fromScalarDataType( dt: omf#ModelScalarDataType ): omf#IRI

  def equivalentScalarDataTypes( dt1: Iterable[omf#ModelScalarDataType], dt2: Iterable[omf#ModelScalarDataType] ): Boolean = {
    val left = dt1.map( fromScalarDataType( _ ) ) toSet
    val right = dt2.map( fromScalarDataType( _ ) ) toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // structured datatype

  def fromStructuredDataType( dt: omf#ModelStructuredDataType ): omf#IRI

  def equivalentStructuredDataTypes( dt1: Iterable[omf#ModelStructuredDataType], dt2: Iterable[omf#ModelStructuredDataType] ): Boolean = {
    val left = dt1.map( fromStructuredDataType( _ ) ) toSet
    val right = dt2.map( fromStructuredDataType( _ ) ) toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // data relationship from entity to scalar

  def fromDataRelationshipFromEntityToScalar( esc: omf#ModelDataRelationshipFromEntityToScalar ): ( omf#IRI, omf#ModelEntityDefinition, omf#ModelScalarDataType )

  // data relationship from entity to structure

  def fromDataRelationshipFromEntityToStructure( est: omf#ModelDataRelationshipFromEntityToStructure ): ( omf#IRI, omf#ModelEntityDefinition, omf#ModelStructuredDataType )

  // data relationship from structure to scalar

  def fromDataRelationshipFromStructureToScalar( esc: omf#ModelDataRelationshipFromStructureToScalar ): ( omf#IRI, omf#ModelStructuredDataType, omf#ModelScalarDataType )

  // data relationship from structure to structure

  def fromDataRelationshipFromStructureToStructure( est: omf#ModelDataRelationshipFromStructureToStructure ): ( omf#IRI, omf#ModelStructuredDataType, omf#ModelStructuredDataType )

  // model term axioms

  def foldTermAxiom[T]( t: omf#ModelTermAxiom )(
    funEntityDefinitionAspectSubClassAxiom: omf#EntityDefinitionAspectSubClassAxiom => T,
    funEntityConceptSubClassAxiom: omf#EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: omf#EntityConceptRestrictionAxiom => T,
    funEntityRelationshipSubClassAxiom: omf#EntityRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestriction: omf#ScalarDataTypeFacetRestriction => T ): T

  // entity definition aspect subclass axiom

  def fromEntityDefinitionAspectSubClassAxiom( ax: omf#EntityDefinitionAspectSubClassAxiom ): ( omf#ModelEntityDefinition, omf#ModelEntityAspect )

  // entity concept subclass axiom

  def fromEntityConceptSubClassAxiom( ax: omf#EntityConceptSubClassAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityConcept )

  // entity concept restriction axiom

  def fromEntityConceptRestrictionAxiom( ax: omf#EntityConceptRestrictionAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityRelationship, omf#ModelEntityDefinition )

  // entity relationship subclass axiom

  def fromEntityRelationshipSubClassAxiom( ax: omf#EntityRelationshipSubClassAxiom ): ( omf#ModelEntityRelationship, omf#ModelEntityRelationship )

  // scalar datatype facet restriction axiom

  def fromScalarDataTypeFacetRestriction( ax: omf#ScalarDataTypeFacetRestriction ): ( omf#ModelScalarDataType, omf#ModelScalarDataType, Iterable[ConstrainingFacet] )

}

trait MutableTerminologyGraphOps[omf <: OMFiri with OMFtbox with OMFstore] extends ImmutableTerminologyGraphOps[omf] {

  def asImmutableTerminologyGraph( g: omf#MutableModelTerminologyGraph )( implicit store: omf#Store ): Try[omf#ImmutableModelTerminologyGraph]

  /**
   * Create a mutable terminology graph partially identified by an IRI and a kind.
   *
   * The complete identity of a graph includes the IRI, kind and imported/extended graphs.
   * For a mutable terminology graph, imported/extended graphs must be specified via `addTerminologyGraphExtension`
   *
   * @since 0.10.0
   */
  def makeTerminologyGraph(
    iri: omf#IRI,
    kind: TerminologyKind )( implicit store: omf#Store ): Try[omf#MutableModelTerminologyGraph]

  /**
   * @since 0.10.0
   */
  def addTerminologyGraphExtension(
    extendingG: omf#MutableModelTerminologyGraph,
    extendedG: omf#ModelTerminologyGraph )( implicit store: omf#Store ): Try[Unit]

  def saveTerminologyGraph(
    g: omf#MutableModelTerminologyGraph )( implicit store: omf#Store ): Try[Unit]

  /**
   * @since 0.10.2
   */
  def saveTerminologyGraph(
    g: omf#MutableModelTerminologyGraph,
    os: OutputStream )( implicit store: omf#Store ): Try[Unit]

  /**
   * Add to a terminology graph a new ModelEntityAspect
   *
   * @param graph: a terminology graph
   * @param aspectName: the name of a new entity aspect
   *
   */
  def addEntityAspect(
    graph: omf#MutableModelTerminologyGraph,
    aspectName: String )( implicit store: omf#Store ): Try[omf#ModelEntityAspect]

  /**
   * Add to a terminology graph a new ModelEntityConcept
   *
   * @param graph: a terminology graph
   * @param conceptName: the name of a new entity concept
   * @param conceptGraphIRI: optionally, the IRI of a new mutable terminology graph for the concept contents
   * @param isAbstract: boolean flag
   * @return A tuple: ( C, CG ) where:
   * C is the new entity concept (in `graph`)
   * CG is a new graph corresponding to `C` (if conceptGraphIRI is provided, otherwise none)
   * @since 0.10.2
   */
  def addEntityConcept(
    graph: omf#MutableModelTerminologyGraph,
    conceptName: String,
    conceptGraphIRI: Option[omf#IRI],
    isAbstract: Boolean = false )( implicit store: omf#Store ): Try[( omf#ModelEntityConcept, Option[omf#MutableModelTerminologyGraph] )]

  /**
   * Add to a terminology graph a new ModelEntityRelationship
   *
   * @param graph: a terminology graph
   * @param source: an existing entity definition that will be the source of the new entity relationship
   * @param target: an existing entity definition that will be the target of the new entity relationship
   * @param characteristics: the characteristics of the new entity relationship
   * @param reifiedRelationshipName: the name of the new entity relationship from the perspective of a reified concept-like entity
   * @param relationshipGraphIRI: optionally, the IRI of a new mutable terminology graph for the relationship contents
   * @param unreifiedRelationshipName: the name of the entity relationship from the perspective of a directed property from the source to the target
   * @param unreifiedInverseRelationshipName: if applicable, the name of the entity relationship from the perspective of a directed inverse property from the target to the source
   * @param isAbstract: boolean flag
   * @return A tuple: ( R, RG ) where:
   * R is the new entity relationship (in `graph`)
   * RG is a new graph corresponding to `R`
   * @since 0.10.2
   */
  def addEntityRelationship(
    graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    relationshipGraphIRI: Option[omf#IRI],
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String] = None,
    isAbstract: Boolean = true )( implicit store: omf#Store ): Try[( omf#ModelEntityRelationship, Option[omf#MutableModelTerminologyGraph] )]

  def addScalarDataType(
    graph: omf#MutableModelTerminologyGraph,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelScalarDataType]

  def addStructuredDataType(
    graph: omf#MutableModelTerminologyGraph,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelStructuredDataType]

  def addDataRelationshipFromEntityToScalar(
    graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelScalarDataType,
    dataRelationshipName: String )( implicit store: omf#Store ): Try[omf#ModelDataRelationshipFromEntityToScalar]

  def addDataRelationshipFromEntityToStructure(
    graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelStructuredDataType,
    dataRelationshipName: String )( implicit store: omf#Store ): Try[omf#ModelDataRelationshipFromEntityToStructure]

  def addDataRelationshipFromStructureToScalar(
    graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelStructuredDataType,
    target: omf#ModelScalarDataType,
    dataRelationshipName: String )( implicit store: omf#Store ): Try[omf#ModelDataRelationshipFromStructureToScalar]

  def addDataRelationshipFromStructureToStructure(
    graph: omf#MutableModelTerminologyGraph,
    source: omf#ModelStructuredDataType,
    target: omf#ModelStructuredDataType,
    dataRelationshipName: String )( implicit store: omf#Store ): Try[omf#ModelDataRelationshipFromStructureToStructure]

  // model term axioms

  // entity definition aspect subclass axiom

  def addEntityDefinitionAspectSubClassAxiom(
    graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityDefinition,
    sup: omf#ModelEntityAspect )( implicit store: omf#Store ): Try[omf#EntityDefinitionAspectSubClassAxiom]

  // entity concept subclass axiom

  def addEntityConceptSubClassAxiom(
    graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    sup: omf#ModelEntityConcept )( implicit store: omf#Store ): Try[omf#EntityConceptSubClassAxiom]

  // entity concept restriction axiom

  def addEntityConceptRestrictionAxiom(
    graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    rel: omf#ModelEntityRelationship,
    range: omf#ModelEntityDefinition )( implicit store: omf#Store ): Try[omf#EntityConceptRestrictionAxiom]

  // entity relationship subclass axiom

  def addEntityRelationshipSubClassAxiom(
    graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelEntityRelationship,
    sup: omf#ModelEntityRelationship )( implicit store: omf#Store ): Try[omf#EntityRelationshipSubClassAxiom]

  // scalar datatype facet restriction axiom

  def addScalarDataTypeFacetRestriction(
    graph: omf#MutableModelTerminologyGraph,
    sub: omf#ModelScalarDataType,
    sup: omf#ModelScalarDataType,
    facets: Iterable[ConstrainingFacet] )( implicit store: omf#Store ): Try[omf#ScalarDataTypeFacetRestriction]

}

trait ImmutableInstanceGraphOps[omf <: OMFiri with OMFabox with OMFtbox with OMFstore] {

  def loadInstanceGraph( iri: omf#IRI )( implicit store: omf#Store ): Try[omf#ImmutableModelInstanceGraph]

  def getInstanceGraphIRI( graph: omf#ModelInstanceGraph ): omf#IRI

  def fromInstanceGraph( graph: omf#ModelInstanceGraph ): ( omf#IRI, Iterable[omf#ImmutableModelTerminologyGraph], Iterable[omf#ModelInstanceGraph], Iterable[omf#ModelInstanceObject], Iterable[omf#ModelInstanceRelation], Iterable[omf#ModelInstanceDataLiteral], Iterable[omf#ModelInstanceDataStructure], Iterable[omf#ModelInstanceDataRelationshipFromEntityToScalar], Iterable[omf#ModelInstanceDataRelationshipFromEntityToStructure], Iterable[omf#ModelInstanceDataRelationshipFromStructureToScalar], Iterable[omf#ModelInstanceDataRelationshipFromStructureToStructure] )

  // instance object

  def fromInstanceObject( o: omf#ModelInstanceObject ): ( omf#IRI, omf#ModelEntityConcept )

  // instance relation

  def fromInstanceRelation( r: omf#ModelInstanceRelation ): ( omf#IRI, omf#ModelEntityRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance )

  // data literal

  def fromDataLiteral( dl: omf#ModelInstanceDataLiteral ): ( String, omf#ModelScalarDataType )

  // data structure

  def fromDataStructure( ds: omf#ModelInstanceDataStructure ): ( omf#IRI, omf#ModelStructuredDataType )

  // data relationship from entity to scalar

  def fromInstanceDataRelationshipFromEntityToScalar( e2sc: omf#ModelInstanceDataRelationshipFromEntityToScalar ): ( omf#ModelEntityInstance, omf#ModelDataRelationshipFromEntityToScalar, omf#ModelInstanceDataLiteral )

  // data relationship from entity to structure

  def fromInstanceDataRelationshipFromEntityToStructure( e2sc: omf#ModelInstanceDataRelationshipFromEntityToStructure ): ( omf#ModelEntityInstance, omf#ModelDataRelationshipFromEntityToStructure, omf#ModelInstanceDataStructure )

  // data relationship from structure to scalar

  def fromInstanceDataRelationshipFromStructureToScalar( e2sc: omf#ModelInstanceDataRelationshipFromStructureToScalar ): ( omf#ModelInstanceDataStructure, omf#ModelDataRelationshipFromStructureToScalar, omf#ModelInstanceDataLiteral )

  // data relationship from structure to structure

  def fromInstanceDataRelationshipFromStructureToStructure( e2sc: omf#ModelInstanceDataRelationshipFromStructureToStructure ): ( omf#ModelInstanceDataStructure, omf#ModelDataRelationshipFromStructureToStructure, omf#ModelInstanceDataStructure )

}

trait MutableInstanceGraphOps[omf <: OMFiri with OMFabox with OMFtbox with OMFstore] extends ImmutableInstanceGraphOps[omf] {

  def asImmutableInstanceGraph( g: omf#MutableModelInstanceGraph )( implicit store: omf#Store ): Try[omf#ImmutableModelInstanceGraph]

  def makeInstanceGraph(
    iri: omf#IRI,
    instantiatedTGraphs: Iterable[omf#ImmutableModelTerminologyGraph],
    extendedIGraphs: Iterable[omf#ImmutableModelInstanceGraph] )( implicit store: omf#Store ): Try[omf#MutableModelInstanceGraph]

  def saveInstanceGraph( g: omf#MutableModelInstanceGraph )( implicit store: omf#Store ): Try[Unit]

  /**
   * @since 0.10.2
   */
  def saveInstanceGraph( g: omf#MutableModelInstanceGraph, os: OutputStream )( implicit store: omf#Store ): Try[Unit]

  // instance object

  def addInstanceObject(
    graph: omf#MutableModelInstanceGraph,
    conceptType: omf#ModelEntityConcept,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceObject]

  // instance relation

  def addInstanceRelation(
    graph: omf#MutableModelInstanceGraph,
    relationshipType: omf#ModelEntityRelationship,
    source: omf#ModelEntityInstance,
    target: omf#ModelEntityInstance,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceRelation]

  // data literal

  def addDataLiteral(
    graph: omf#MutableModelInstanceGraph,
    datatype: omf#ModelScalarDataType,
    lexicalForm: String )( implicit store: omf#Store ): Try[omf#ModelInstanceDataLiteral]

  // data structure

  def addDataStructure(
    graph: omf#MutableModelInstanceGraph,
    datatype: omf#ModelStructuredDataType,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceDataStructure]

  // data relationship from entity to scalar

  def addInstanceDataRelationshipFromEntityToScalar(
    graph: omf#MutableModelInstanceGraph,
    ei: omf#ModelEntityInstance,
    e2sc: omf#ModelDataRelationshipFromEntityToScalar,
    value: omf#ModelInstanceDataLiteral )( implicit store: omf#Store ): Try[omf#ModelInstanceDataRelationshipFromEntityToScalar]

  // data relationship from entity to structure

  def addInstanceDataRelationshipFromEntityToStructure(
    graph: omf#MutableModelInstanceGraph,
    ei: omf#ModelEntityInstance,
    e2st: omf#ModelDataRelationshipFromEntityToStructure,
    value: omf#ModelInstanceDataStructure )( implicit store: omf#Store ): Try[omf#ModelInstanceDataRelationshipFromEntityToStructure]

  // data relationship from structure to scalar

  def addInstanceDataRelationshipFromStructureToScalar(
    graph: omf#MutableModelInstanceGraph,
    di: omf#ModelInstanceDataStructure,
    e2sc: omf#ModelDataRelationshipFromStructureToScalar,
    value: omf#ModelInstanceDataLiteral )( implicit store: omf#Store ): Try[omf#ModelInstanceDataRelationshipFromStructureToScalar]

  // data relationship from structure to structure

  def addInstanceDataRelationshipFromStructureToStructure(
    graph: omf#MutableModelInstanceGraph,
    di: omf#ModelInstanceDataStructure,
    e2st: omf#ModelDataRelationshipFromStructureToStructure,
    value: omf#ModelInstanceDataStructure )( implicit store: omf#Store ): Try[omf#ModelInstanceDataRelationshipFromStructureToStructure]

}

trait OMFOps[omf <: OMF]
  extends IRIOps[omf]
  with MutableTerminologyGraphOps[omf]
  with MutableInstanceGraphOps[omf] 
