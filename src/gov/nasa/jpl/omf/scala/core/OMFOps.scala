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
  extends OMFDSL[omf]
  with syntax.OMFSyntax[omf] {

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

  def tboxIsomorphism( g1: omf#ModelTerminologyGraph, g2: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri1, c1, r1, sc1, st1, sdr1, edr1, ax1 ) = fromTerminologyGraph( g1 )
    val ( iri2, c2, r2, sc2, st2, sdr2, edr2, ax2 ) = fromTerminologyGraph( g2 )
    if ( iri1 != iri2 ) return false
    if ( !equivalentEntityConcepts( c1, c2 ) ) return false
    if ( !equivalentEntityRelationships( r1, r2 ) ) return false
    if ( !equivalentScalarDataTypes( sc1, sc2 ) ) return false
    if ( !equivalentStructuredDataTypes( st1, st2 ) ) return false
    if ( !equivalentStructuredDataRelationships( sdr1, sdr2 ) ) return false
    if ( !equivalentEntityDataRelationships( edr1, edr2 ) ) return false
    // @TODO check for axiom equivalence...
    true
  }

  def makeTerminologyGraph( iri: omf#IRI ): omf#ModelTerminologyGraph = makeTerminologyGraph( iri, c = Nil, r = Nil, sc = Nil, st = Nil, sdr = Nil, edr = Nil, ax = Nil )

  def makeTerminologyGraph(
    iri: omf#IRI,
    c: Iterable[omf#ModelEntityConcept],
    r: Iterable[omf#ModelEntityRelationship],
    sc: Iterable[omf#ModelScalarDataType],
    st: Iterable[omf#ModelStructuredDataType],
    sdr: Iterable[omf#ModelStructuredDataRelationship],
    edr: Iterable[omf#ModelEntityDataRelationship],
    ax: Iterable[omf#ModelTermAxiom] ): omf#ModelTerminologyGraph

  def getTerminologyGraphIRI( graph: omf#ModelTerminologyGraph ): omf#IRI

  def fromTerminologyGraph( graph: omf#ModelTerminologyGraph ): ( omf#IRI, Iterable[omf#ModelEntityConcept], Iterable[omf#ModelEntityRelationship], Iterable[omf#ModelScalarDataType], Iterable[omf#ModelStructuredDataType], Iterable[omf#ModelStructuredDataRelationship], Iterable[omf#ModelEntityDataRelationship], Iterable[omf#ModelTermAxiom] )

  def isEntityDefinitionInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _c.toSet contains t ) || ( _r.toSet contains t )
  }

  def isEntityTypeInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _c.toSet contains t ) || ( _r.toSet contains t )
  }

  def isDataDefinitionInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _sc.toSet contains t ) || ( _st.toSet contains t )
  }

  def isDataTypeInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean = {
    val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
    ( _sc.toSet contains t ) || ( _st.toSet contains t )
  }

  def isTypeTermInTerminologyGraph( t: omf#ModelTypeTerm, graph: omf#ModelTerminologyGraph ): Boolean =
    isEntityTypeInTerminologyGraph( t, graph ) ||
      isDataTypeInTerminologyGraph( t, graph ) || {
        val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( graph )
        ( _sdr.toSet contains t ) || ( _edr.toSet contains t )
      }

  def lookupTypeTerm( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelTypeTerm]

  def lookupEntityDefinition( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityDefinition]
  def lookupEntityConcept( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityConcept]
  def lookupEntityRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityRelationship]
  def lookupScalarDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelScalarDataType]
  def lookupStructuredDataType( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelStructuredDataType]
  def lookupStructuredDataRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelStructuredDataRelationship]
  def lookupEntityDataRelationship( graph: omf#ModelTerminologyGraph, iri: omf#IRI ): Option[omf#ModelEntityDataRelationship]

  def resolveTerminologyGraphFragmentToNewIRI( tbox: Try[omf#ModelTerminologyGraph], fragment: String ): Try[( omf#ModelTerminologyGraph, omf#IRI )] = tbox match {
    case Failure( t ) => Failure( t )
    case Success( g ) => withFragment( getTerminologyGraphIRI( g ), fragment ) match {
      case Failure( t ) => Failure( t )
      case Success( iri ) => lookupTypeTerm( g, iri ) match {
        case None      => Success( ( g, iri ) )
        case Some( x ) => Failure( new IllegalArgumentException( s" IRI conflict: the fragment '${fragment}' conflicts with an existing term: ${x}" ) )
      }
    }
  }

  def resolveTerminologyGraphFragmentToEntityType( tbox: Try[omf#ModelTerminologyGraph], fragment: String ): Try[( omf#ModelTerminologyGraph, omf#ModelEntityDefinition )] = tbox match {
    case Failure( t ) => Failure( t )
    case Success( g ) => withFragment( getTerminologyGraphIRI( g ), fragment ) match {
      case Failure( t ) => Failure( t )
      case Success( iri ) => lookupEntityDefinition( g, iri ) match {
        case None       => Failure( new IllegalArgumentException( s" No entity definition with IRI: ${iri}" ) )
        case Some( et ) => Success( ( g, et ) )
      }
    }
  }

  def resolveTerminologyGraphEntityType( tbox: Try[omf#ModelTerminologyGraph], et: omf#ModelEntityDefinition ): Try[( omf#ModelTerminologyGraph, omf#ModelEntityDefinition )] = tbox match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      if ( isEntityTypeInTerminologyGraph( et, g ) ) Success( ( g, et ) )
      else Failure( new IllegalArgumentException( s"Entity type ${et} is not in the graph ${g}" ) )
  }

  def resolveTerminologyGraphFragmentToEntityDefinition( tbox: Try[omf#ModelTerminologyGraph], fragment: String ): Try[( omf#ModelTerminologyGraph, omf#ModelEntityDefinition )] = tbox match {
    case Failure( t ) => Failure( t )
    case Success( g ) => withFragment( getTerminologyGraphIRI( g ), fragment ) match {
      case Failure( t ) => Failure( t )
      case Success( iri ) => lookupEntityDefinition( g, iri ) match {
        case None       => Failure( new IllegalArgumentException( s" No entity definition with IRI: ${iri}" ) )
        case Some( ed ) => Success( ( g, ed ) )
      }
    }
  }

  def resolveTerminologyGraphEntityDefinition( tbox: Try[omf#ModelTerminologyGraph], ed: omf#ModelEntityDefinition ): Try[( omf#ModelTerminologyGraph, omf#ModelEntityDefinition )] = tbox match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      if ( isEntityDefinitionInTerminologyGraph( ed, g ) ) Success( ( g, ed ) )
      else Failure( new IllegalArgumentException( s"Entity definitoin ${ed} is not in the graph ${g}" ) )
  }

  def addEntityConcept( graph: Try[omf#ModelTerminologyGraph], c: omf#ModelEntityConcept ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromEntityConcept( c ) ) match {
        case None => Success( makeTerminologyGraph( iri, _c ++ Seq( c ), _r, _sc, _st, _sdr, _edr, _ax ) )
        case Some( t ) => if ( c == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new entity concept ${c} and existing term: ${t}" ) )
      }
  }

  def addEntityRelationship( graph: Try[omf#ModelTerminologyGraph], r: omf#ModelEntityRelationship ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromEntityRelationship( r )._1 ) match {
        case None => Success( makeTerminologyGraph( iri, _c, _r ++ Seq( r ), _sc, _st, _sdr, _edr, _ax ) )
        case Some( t ) => if ( r == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new entity relationship ${r} and existing term: ${t}" ) )
      }
  }

  def addScalarDataType( graph: Try[omf#ModelTerminologyGraph], sc: omf#ModelScalarDataType ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromScalarDataType( sc ) ) match {
        case None => Success( makeTerminologyGraph( iri, _c, _r, _sc ++ Seq( sc ), _st, _sdr, _edr, _ax ) )
        case Some( t ) => if ( sc == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new scalar datatype ${sc} and existing term: ${t}" ) )
      }
  }

  def addStructuredDataType( graph: Try[omf#ModelTerminologyGraph], st: omf#ModelStructuredDataType ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromStructuredDataType( st ) ) match {
        case None => Success( makeTerminologyGraph( iri, _c, _r, _sc, _st ++ Seq( st ), _sdr, _edr, _ax ) )
        case Some( t ) => if ( st == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new structured datatype ${st} and existing term: ${t}" ) )
      }
  }

  def addStructuredDataRelationship( graph: Try[omf#ModelTerminologyGraph], sdr: omf#ModelStructuredDataRelationship ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromStructuredDataRelationship( sdr )._1 ) match {
        case None => Success( makeTerminologyGraph( iri, _c, _r, _sc, _st, _sdr ++ Seq( sdr ), _edr, _ax ) )
        case Some( t ) => if ( sdr == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new structured data relationhsip ${sdr} and existing term: ${t}" ) )
      }
  }

  def addEntityDataRelationship( graph: Try[omf#ModelTerminologyGraph], edr: omf#ModelEntityDataRelationship ): Try[omf#ModelTerminologyGraph] = graph match {
    case Failure( t ) => Failure( t )
    case Success( g ) =>
      val ( iri, _c, _r, _sc, _st, _sdr, _edr, _ax ) = fromTerminologyGraph( g )
      lookupTypeTerm( g, fromEntityDataRelationship( edr )._1 ) match {
        case None => Success( makeTerminologyGraph( iri, _c, _r, _sc, _st, _sdr, _edr ++ Seq( edr ), _ax ) )
        case Some( t ) => if ( edr == t ) Success( g )
        else Failure( new IllegalArgumentException( s"IRI conflict between new entity data relationhsip ${edr} and existing term: ${t}" ) )
      }
  }

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

  // entity definition

  def fromEntityDefinition( e: omf#ModelEntityDefinition ): omf#IRI

  // entity concept

  def makeEntityConcept( iri: omf#IRI ): omf#ModelEntityConcept

  def fromEntityConcept( c: omf#ModelEntityConcept ): omf#IRI

  def equivalentEntityConcepts( c1: Iterable[omf#ModelEntityConcept], c2: Iterable[omf#ModelEntityConcept] ): Boolean = {
    val iris1 = c1.map( fromEntityConcept( _ ) ) toSet
    val iris2 = c2.map( fromEntityConcept( _ ) ) toSet
    val d = iris1.diff( iris2 )
    d.isEmpty
  }

  // entity relationship

  def makeEntityRelationship(
    iri: omf#IRI,
    source: omf#ModelEntityDefinition,
    target: omf#ModelEntityDefinition,
    characteristics: RelationshipCharacteristics* ): omf#ModelEntityRelationship =
    makeEntityRelationship( iri, source, target, characteristics.toIterable )

  def makeEntityRelationship(
    iri: omf#IRI,
    source: omf#ModelEntityDefinition,
    target: omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics] ): omf#ModelEntityRelationship

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

  def makeScalarDataType( iri: omf#IRI ): omf#ModelScalarDataType

  def fromScalarDataType( dt: omf#ModelScalarDataType ): omf#IRI

  def equivalentScalarDataTypes( dt1: Iterable[omf#ModelScalarDataType], dt2: Iterable[omf#ModelScalarDataType] ): Boolean = {
    val left = dt1.map( fromScalarDataType( _ ) ) toSet
    val right = dt2.map( fromScalarDataType( _ ) ) toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // structured datatype

  def makeStructuredDataType( iri: omf#IRI ): omf#ModelStructuredDataType

  def fromStructuredDataType( dt: omf#ModelStructuredDataType ): omf#IRI

  def equivalentStructuredDataTypes( dt1: Iterable[omf#ModelStructuredDataType], dt2: Iterable[omf#ModelStructuredDataType] ): Boolean = {
    val left = dt1.map( fromStructuredDataType( _ ) ) toSet
    val right = dt2.map( fromStructuredDataType( _ ) ) toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // structured data relationship

  def makeStructuredDataRelationship( iri: omf#IRI, source: omf#ModelStructuredDataType, target: omf#ModelDataTypeDefinition ): omf#ModelStructuredDataRelationship

  def fromStructuredDataRelationship( sd: omf#ModelStructuredDataRelationship ): ( omf#IRI, omf#ModelStructuredDataType, omf#ModelDataTypeDefinition )

  def equivalentStructuredDataRelationships( sdr1: Iterable[omf#ModelStructuredDataRelationship], sdr2: Iterable[omf#ModelStructuredDataRelationship] ): Boolean = {
    val left = sdr1.map { c => val ( i, s, r ) = fromStructuredDataRelationship( c ); ( i, fromStructuredDataType( s ), fromDataTypeDefinition( r ) ) } toSet
    val right = sdr2.map { c => val ( i, s, r ) = fromStructuredDataRelationship( c ); ( i, fromStructuredDataType( s ), fromDataTypeDefinition( r ) ) } toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // entity data relationship

  def makeEntityDataRelationship( iri: omf#IRI, source: omf#ModelEntityDefinition, target: omf#ModelDataTypeDefinition ): omf#ModelEntityDataRelationship

  def fromEntityDataRelationship( ed: omf#ModelEntityDataRelationship ): ( omf#IRI, omf#ModelEntityDefinition, omf#ModelDataTypeDefinition )

  def equivalentEntityDataRelationships( edr1: Iterable[omf#ModelEntityDataRelationship], edr2: Iterable[omf#ModelEntityDataRelationship] ): Boolean = {
    val left = edr1.map { c => val ( i, s, r ) = fromEntityDataRelationship( c ); ( i, fromEntityDefinition( s ), fromDataTypeDefinition( r ) ) } toSet
    val right = edr2.map { c => val ( i, s, r ) = fromEntityDataRelationship( c ); ( i, fromEntityDefinition( s ), fromDataTypeDefinition( r ) ) } toSet
    val d = left.diff( right )
    d.isEmpty
  }

  // model term axiom

  // entity concept subclass axiom

  def makeEntityConceptSubClassAxiom( sub: omf#ModelEntityConcept, sup: omf#ModelEntityConcept ): omf#EntityConceptSubClassAxiom
  def fromEntityConceptSubClassAxiom( ax: omf#EntityConceptSubClassAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityConcept )

  // entity concept restriction axiom

  def makeEntityConceptRestrictionAxiom( sub: omf#ModelEntityConcept, rel: omf#ModelEntityRelationship, range: omf#ModelEntityDefinition ): omf#EntityConceptRestrictionAxiom
  def fromEntityConceptRestrictionAxiom( ax: omf#EntityConceptRestrictionAxiom ): ( omf#ModelEntityConcept, omf#ModelEntityRelationship, omf#ModelEntityDefinition )

  // entity relationship subclass axiom

  def makeEntityRelationshipSubClassAxiom( sub: omf#ModelEntityRelationship, sup: omf#ModelEntityRelationship ): omf#EntityRelationshipSubClassAxiom
  def fromEntityRelationshipSubClassAxiom( ax: omf#EntityRelationshipSubClassAxiom ): ( omf#ModelEntityRelationship, omf#ModelEntityRelationship )

  // scalar datatype facet restriction axiom

  def makeScalarDataTypeFacetRestriction( sub: omf#ModelScalarDataType, sup: omf#ModelScalarDataType, facets: Iterable[ConstrainingFacet] ): omf#ScalarDataTypeFacetRestriction
  def fromScalarDataTypeFacetRestriction( ax: omf#ScalarDataTypeFacetRestriction ): ( omf#ModelScalarDataType, omf#ModelScalarDataType, Iterable[ConstrainingFacet] )

  def foldTermAxiom[T]( t: omf#ModelTermAxiom )(
    funEntityConceptSubClassAxiom: omf#EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: omf#EntityConceptRestrictionAxiom => T,
    funEntityRelationshipSubClassAxiom: omf#EntityRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestriction: omf#ScalarDataTypeFacetRestriction => T ): T

  // instance graph

  def makeInstanceGraph( iri: omf#IRI,
                         t: Iterable[omf#ModelTerminologyGraph],
                         c: Iterable[omf#ModelInstanceObject],
                         r: Iterable[omf#ModelInstanceRelation],
                         dl: Iterable[omf#ModelInstanceDataLiteral],
                         ic: Iterable[omf#ModelInstanceDataStructure],
                         sdp: Iterable[omf#ModelStructuredDataProperty],
                         edp: Iterable[omf#ModelEntityDataProperty] ): omf#ModelInstanceGraph

  def getInstanceGraphIRI( graph: omf#ModelInstanceGraph ): omf#IRI

  def fromInstanceGraph( graph: omf#ModelInstanceGraph ): ( omf#IRI, Iterable[omf#ModelTerminologyGraph], Iterable[omf#ModelInstanceObject], Iterable[omf#ModelInstanceRelation], Iterable[omf#ModelInstanceDataLiteral], Iterable[omf#ModelInstanceDataStructure], Iterable[omf#ModelStructuredDataProperty], Iterable[omf#ModelEntityDataProperty] )

  // instance object

  def makeInstanceObject( iri: omf#IRI, conceptType: omf#ModelEntityConcept ): omf#ModelInstanceObject

  def fromInstanceObject( o: omf#ModelInstanceObject ): ( omf#IRI, omf#ModelEntityConcept )

  // instance relation

  def makeInstanceRelation( iri: omf#IRI, relationshipType: omf#ModelEntityRelationship, source: omf#ModelEntityInstance, target: omf#ModelEntityInstance ): omf#ModelInstanceRelation

  def fromInstanceRelation( r: omf#ModelInstanceRelation ): ( omf#IRI, omf#ModelEntityRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance )

  // data literal

  def makeDataLiteral( lexicalForm: String, datatype: omf#ModelScalarDataType ): omf#ModelInstanceDataLiteral

  def fromDataLiteral( dl: omf#ModelInstanceDataLiteral ): ( String, omf#ModelScalarDataType )

  // data structure

  def makeDataStructure( iri: omf#IRI, datatype: omf#ModelStructuredDataType ): omf#ModelInstanceDataStructure

  def fromDataStructure( ds: omf#ModelInstanceDataStructure ): ( omf#IRI, omf#ModelStructuredDataType )

  // structured data property

  def makeStructuredDataProperty( ds: omf#ModelInstanceDataStructure, structuredDataRelationshipType: omf#ModelStructuredDataRelationship, value: omf#ModelDataInstance ): omf#ModelStructuredDataProperty

  def fromStructuredDataProperty( sdp: omf#ModelStructuredDataProperty ): ( omf#ModelInstanceDataStructure, omf#ModelStructuredDataRelationship, omf#ModelDataInstance )

  // entity data property

  def makeEntityDataProperty( e: omf#ModelEntityInstance, entityDataRelationshipType: omf#ModelEntityDataRelationship, value: omf#ModelDataInstance ): omf#ModelEntityDataProperty

  def fromEntityDataProperty( edp: omf#ModelEntityDataProperty ): ( omf#ModelEntityInstance, omf#ModelEntityDataRelationship, omf#ModelDataInstance )

}