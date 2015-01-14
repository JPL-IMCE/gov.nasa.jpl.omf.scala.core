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

import scala.language.postfixOps
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object OMFOps {

  def apply[omf <: OMF]( implicit ops: OMFOps[omf] ): OMFOps[omf] = ops

}

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
trait OMFOps[omf <: OMF]
  extends OMFDSL[omf] {

  // IRI

  def makeIRI( s: String ): omf#IRI

  def withFragment( iri: omf#IRI, fragment: String ): Try[omf#IRI]

  def splitIRI( iri: omf#IRI ): ( omf#IRI, Option[String] )

  def fromIRI( iri: omf#IRI ): String

  def toBackboneIRI( iri: omf#IRI ): omf#IRI

  def toObjectPropertyIRI( iri: omf#IRI ): omf#IRI

  def toSourceIRI( iri: omf#IRI ): omf#IRI

  def toTargetIRI( iri: omf#IRI ): omf#IRI

  // terminology graph

  def loadTerminologyGraph( iri: omf#IRI )( implicit store: omf#Store ): Try[omf#ModelTerminologyGraph]

  def makeTerminologyGraph(
    iri: omf#IRI,
    extendedTGraphs: Iterable[omf#ModelTerminologyGraph] )( implicit store: omf#Store ): Try[omf#ModelTerminologyGraph]

  def getTerminologyGraphIRI( graph: omf#ModelTerminologyGraph ): omf#IRI

  def fromTerminologyGraph( graph: omf#ModelTerminologyGraph ): ( omf#IRI, Iterable[omf#ModelTerminologyGraph], 
      Iterable[omf#ModelEntityAspect], 
      Iterable[omf#ModelEntityConcept], Iterable[omf#ModelEntityRelationship], Iterable[omf#ModelScalarDataType], Iterable[omf#ModelStructuredDataType], Iterable[omf#ModelStructuredDataRelationship], Iterable[omf#ModelEntityDataRelationship], Iterable[omf#ModelTermAxiom] )

  def isEntityDefinitionAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _i, _f, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _c.toSet contains t ) || ( _r.toSet contains t )
  }

  def isEntityDefinitionImportedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    !isEntityDefinitionAssertedInTerminologyGraph( t, graph ) && {
      val ( iri, _i, _f, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
      _i.exists( ig => isEntityDefinitionAssertedInTerminologyGraph( t, ig ) || isEntityDefinitionImportedInTerminologyGraph( t, ig ) )
    }
  }

  def isEntityDataRelationshipAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _i, _f, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _edr.toSet contains t )
  }

  def isDataDefinitionAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _i, _f, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _sc.toSet contains t ) || ( _st.toSet contains t )
  }

  def isStructuredDataRelationshipAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _i, _f, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _sdr.toSet contains t )
  }

  def isTypeTermAssertedInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean =
    isEntityDefinitionAssertedInTerminologyGraph( t, graph ) ||
      isEntityDataRelationshipAssertedInTerminologyGraph( t, graph ) ||
      isDataDefinitionAssertedInTerminologyGraph( t, graph ) ||
      isStructuredDataRelationshipAssertedInTerminologyGraph( t, graph )

  def lookupTypeTerm( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelTypeTerm]

  def lookupEntityDefinition( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityDefinition]
  def lookupEntityConcept( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityConcept]
  def lookupEntityRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityRelationship]
  def lookupScalarDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelScalarDataType]
  def lookupStructuredDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelStructuredDataType]
  def lookupStructuredDataRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelStructuredDataRelationship]
  def lookupEntityDataRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityDataRelationship]

  /**
   * Add to a terminology graph a new ModelEntityAspect
   *
   * @param graph: a terminology graph
   * @param aspectName: the name of a new entity aspect
   *
   */
  def addEntityAspect(
    graph: omf#ModelTerminologyGraph,
    aspectName: String )( implicit store: omf#Store ): Try[omf#ModelEntityAspect]

  /**
   * Add to a terminology graph a new ModelEntityConcept
   *
   * @param graph: a terminology graph
   * @param conceptName: the name of a new entity concept
   * @param isAbstract: boolean flag
   */
  def addEntityConcept(
    graph: omf#ModelTerminologyGraph,
    conceptName: String,
    isAbstract: Boolean = false )( implicit store: omf#Store ): Try[omf#ModelEntityConcept]

  /**
   * Add to a terminology graph a new ModelEntityRelationship
   *
   * @param graph: a terminology graph
   * @param source: an existing entity definition that will be the source of the new entity relationship
   * @param target: an existing entity definition that will be the target of the new entity relationship
   * @param characteristics: the characteristics of the new entity relationship
   * @param reifiedRelationshipName: the name of the new entity relationship from the perspective of a reified concept-like entity
   * @param unreifiedRelationshipName: the name of the entity relationship from the perspective of a directed property from the source to the target
   * @param unreifiedInverseRelationshipName: if applicable, the name of the entity relationship from the perspective of a directed inverse property from the target to the source
   * @param isAbstract: boolean flag
   */
  def addEntityRelationship(
    graph: omf#ModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String] = None,
    isAbstract: Boolean = true )( implicit store: omf#Store ): Try[omf#ModelEntityRelationship]

  def addScalarDataType(
    graph: omf#ModelTerminologyGraph,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelScalarDataType]

  def addStructuredDataType(
    graph: omf#ModelTerminologyGraph,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelStructuredDataType]

  def addStructuredDataRelationship(
    graph: omf#ModelTerminologyGraph,
    source: omf#ModelStructuredDataType,
    target: omf#ModelDataTypeDefinition,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelStructuredDataRelationship]

  def addEntityDataRelationship(
    graph: omf#ModelTerminologyGraph,
    source: omf#ModelEntityDefinition,
    target: omf#ModelDataTypeDefinition,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelEntityDataRelationship]

  def getTerms( graph: omf#ModelTerminologyGraph ): ( omf#IRI, Iterable[omf#ModelTypeTerm] )

  def foldTerm[T]( t: omf#ModelTypeTerm )(
    funEntityConcept: omf#ModelEntityConcept => T,
    funEntityRelationship: omf#ModelEntityRelationship => T,
    funScalarDataType: omf#ModelScalarDataType => T,
    funStructuredDataType: omf#ModelStructuredDataType => T,
    funStructuredDataRelationship: omf#ModelStructuredDataRelationship => T,
    funEntityDataRelationship: omf#ModelEntityDataRelationship => T ): T

  def fromTerm( t: omf#ModelTypeTerm ): omf#IRI = foldTerm[omf#IRI]( t )(
    ( ec: omf#ModelEntityConcept ) => fromEntityConcept( ec ),
    ( er: omf#ModelEntityRelationship ) => fromEntityRelationship( er )._1,
    ( sc: omf#ModelScalarDataType ) => fromScalarDataType( sc ),
    ( sd: omf#ModelStructuredDataType ) => fromStructuredDataType( sd ),
    ( sdr: omf#ModelStructuredDataRelationship ) => fromStructuredDataRelationship( sdr )._1,
    ( edr: omf#ModelEntityDataRelationship ) => fromEntityDataRelationship( edr )._1 )

  // entity aspect

  def fromEntityASpect( t: omf#ModelEntityAspect ): omf#IRI
  
  // entity definition

  def fromEntityDefinition( e: omf#ModelEntityDefinition ): omf#IRI

  // entity concept

  def fromEntityConcept( c: omf#ModelEntityConcept ): omf#IRI

  def equivalentEntityConcepts( c1: Iterable[omf#ModelEntityConcept], c2: Iterable[omf#ModelEntityConcept] ): Boolean = {
    val iris1 = c1.map( fromEntityConcept( _ ) ) toSet
    val iris2 = c2.map( fromEntityConcept( _ ) ) toSet
    val d = iris1.diff( iris2 )
    d.isEmpty
  }

  // entity relationship

  def fromEntityRelationship( r: omf#ModelEntityRelationship ): ( omf#IRI, omf#ModelEntityDefinition, omf#ModelEntityDefinition, Iterable[RelationshipCharacteristics] )

  def equivalentEntityRelationships( r1: Iterable[omf#ModelEntityRelationship], r2: Iterable[omf#ModelEntityRelationship] ): Boolean = {
    val left = r1.map { r => val ( i, s, t, c ) = fromEntityRelationship( r ); ( i, fromEntityDefinition( s ), fromEntityDefinition( t ), relationshipCharacteristicsSummary( c ) ) } toSet
    val right = r2.map { r => val ( i, s, t, c ) = fromEntityRelationship( r ); ( i, fromEntityDefinition( s ), fromEntityDefinition( t ), relationshipCharacteristicsSummary( c ) ) } toSet
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

  // structured data relationship

  def fromStructuredDataRelationship( sd: omf#ModelStructuredDataRelationship ): ( omf#IRI, omf#ModelStructuredDataType, omf#ModelDataTypeDefinition )

  def equivalentStructuredDataRelationships( sdr1: Iterable[omf#ModelStructuredDataRelationship], sdr2: Iterable[omf#ModelStructuredDataRelationship] ): Boolean = {
    val left = sdr1.map { c => val ( i, s, r ) = fromStructuredDataRelationship( c ); ( i, fromStructuredDataType( s ), fromDataTypeDefinition( r ) ) } toSet
    val right = sdr2.map { c => val ( i, s, r ) = fromStructuredDataRelationship( c ); ( i, fromStructuredDataType( s ), fromDataTypeDefinition( r ) ) } toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // entity data relationship

  def fromEntityDataRelationship( ed: omf#ModelEntityDataRelationship ): ( omf#IRI, omf#ModelEntityDefinition, omf#ModelDataTypeDefinition )

  def equivalentEntityDataRelationships( edr1: Iterable[omf#ModelEntityDataRelationship], edr2: Iterable[omf#ModelEntityDataRelationship] ): Boolean = {
    val left = edr1.map { c => val ( i, s, r ) = fromEntityDataRelationship( c ); ( i, fromEntityDefinition( s ), fromDataTypeDefinition( r ) ) } toSet
    val right = edr2.map { c => val ( i, s, r ) = fromEntityDataRelationship( c ); ( i, fromEntityDefinition( s ), fromDataTypeDefinition( r ) ) } toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // model term axiom

  // entity definition aspect subclass axiom
  
  def addEntityDefinitionAspectSubClassAxiom(
    graph: omf#ModelTerminologyGraph,
    sub: omf#ModelEntityDefinition,
    sup: omf#ModelEntityAspect )( implicit store: omf#Store ): Try[omf#EntityDefinitionAspectSubClassAxiom]
  def fromEntityDefinitionAspectSubClassAxiom( ax: omf#EntityDefinitionAspectSubClassAxiom ): ( omf#ModelEntityDefinition, omf#ModelEntityAspect )
  
  // entity concept subclass axiom

  def addEntityConceptSubClassAxiom(
    graph: omf#ModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    sup: omf#ModelEntityConcept )( implicit store: omf#Store ): Try[omf#EntityConceptSubClassAxiom]
  def fromEntityConceptSubClassAxiom( ax: omf#EntityConceptSubClassAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityConcept )

  // entity concept restriction axiom

  def addEntityConceptRestrictionAxiom(
    graph: omf#ModelTerminologyGraph,
    sub: omf#ModelEntityConcept,
    rel: omf#ModelEntityRelationship,
    range: omf#ModelEntityDefinition )( implicit store: omf#Store ): Try[omf#EntityConceptRestrictionAxiom]
  def fromEntityConceptRestrictionAxiom( ax: omf#EntityConceptRestrictionAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityRelationship, omf#ModelEntityDefinition )

  // entity relationship subclass axiom

  def addEntityRelationshipSubClassAxiom(
    graph: omf#ModelTerminologyGraph,
    sub: omf#ModelEntityRelationship,
    sup: omf#ModelEntityRelationship )( implicit store: omf#Store ): Try[omf#EntityRelationshipSubClassAxiom]
  def fromEntityRelationshipSubClassAxiom( ax: omf#EntityRelationshipSubClassAxiom ): ( omf#ModelEntityRelationship, omf#ModelEntityRelationship )

  // scalar datatype facet restriction axiom

  def addScalarDataTypeFacetRestriction( graph: omf#ModelTerminologyGraph, sub: omf#ModelScalarDataType, sup: omf#ModelScalarDataType, facets: Iterable[ConstrainingFacet] )( implicit store: omf#Store ): Try[omf#ScalarDataTypeFacetRestriction]
  def fromScalarDataTypeFacetRestriction( ax: omf#ScalarDataTypeFacetRestriction ): ( omf#ModelScalarDataType, omf#ModelScalarDataType, Iterable[ConstrainingFacet] )

  def foldTermAxiom[T]( t: omf#ModelTermAxiom )(
    funEntityConceptSubClassAxiom: omf#EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: omf#EntityConceptRestrictionAxiom => T,
    funEntityRelationshipSubClassAxiom: omf#EntityRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestriction: omf#ScalarDataTypeFacetRestriction => T ): T

  // instance graph

  def loadInstanceGraph( iri: omf#IRI )( implicit store: omf#Store ): Try[omf#ModelInstanceGraph]

  def makeInstanceGraph(
    iri: omf#IRI,
    instantiatedTGraphs: Iterable[omf#ModelTerminologyGraph],
    extendedIGraphs: Iterable[omf#ModelInstanceGraph] )( implicit store: omf#Store ): Try[omf#ModelInstanceGraph]

  def getInstanceGraphIRI( graph: omf#ModelInstanceGraph ): omf#IRI

  def fromInstanceGraph( graph: omf#ModelInstanceGraph ): ( omf#IRI, Iterable[omf#ModelTerminologyGraph], Iterable[omf#ModelInstanceGraph], Iterable[omf#ModelInstanceObject], Iterable[omf#ModelInstanceRelation], Iterable[omf#ModelInstanceDataLiteral], Iterable[omf#ModelInstanceDataStructure], Iterable[omf#ModelStructuredDataProperty], Iterable[omf#ModelEntityDataProperty] )

  // instance object

  def addInstanceObject(
    graph: omf#ModelInstanceGraph,
    conceptType: omf#ModelEntityConcept,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceObject]

  def fromInstanceObject( o: omf#ModelInstanceObject ): ( omf#IRI, omf#ModelEntityConcept )

  // instance relation

  def addInstanceRelation(
    graph: omf#ModelInstanceGraph,
    relationshipType: omf#ModelEntityRelationship,
    source: omf#ModelEntityInstance,
    target: omf#ModelEntityInstance,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceRelation]

  def fromInstanceRelation( r: omf#ModelInstanceRelation ): ( omf#IRI, omf#ModelEntityRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance )

  // data literal

  def addDataLiteral(
    graph: omf#ModelInstanceGraph,
    datatype: omf#ModelScalarDataType,
    lexicalForm: String )( implicit store: omf#Store ): Try[omf#ModelInstanceDataLiteral]

  def fromDataLiteral( dl: omf#ModelInstanceDataLiteral ): ( String, omf#ModelScalarDataType )

  // data structure

  def addDataStructure(
    graph: omf#ModelInstanceGraph,
    datatype: omf#ModelStructuredDataType,
    fragment: String )( implicit store: omf#Store ): Try[omf#ModelInstanceDataStructure]

  def fromDataStructure( ds: omf#ModelInstanceDataStructure ): ( omf#IRI, omf#ModelStructuredDataType )

  // structured data property

  def addStructuredDataProperty(
    graph: omf#ModelInstanceGraph,
    ds: omf#ModelInstanceDataStructure,
    structuredDataRelationshipType: omf#ModelStructuredDataRelationship,
    value: omf#ModelDataInstance )( implicit store: omf#Store ): Try[omf#ModelStructuredDataProperty]

  def fromStructuredDataProperty( sdp: omf#ModelStructuredDataProperty ): ( omf#ModelInstanceDataStructure, omf#ModelStructuredDataRelationship, omf#ModelDataInstance )

  // entity data property

  def addEntityDataProperty(
    graph: omf#ModelInstanceGraph,
    e: omf#ModelEntityInstance,
    entityDataRelationshipType: omf#ModelEntityDataRelationship,
    value: omf#ModelDataInstance )( implicit store: omf#Store ): Try[omf#ModelEntityDataProperty]

  def fromEntityDataProperty( edp: omf#ModelEntityDataProperty ): ( omf#ModelEntityInstance, omf#ModelEntityDataRelationship, omf#ModelDataInstance )

}