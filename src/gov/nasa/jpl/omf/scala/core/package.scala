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
package gov.nasa.jpl.omf.scala

package object core {

  /**
   * The reflexive transitive closure of imported terminology graphs
   * @param g The terminology graph whose direct & indirect imports are included (subject to kind filtering)
   * @param onlySameKind determines the filtering of imported terminology graphs
   * @return gs: if onlySameKind is true; then gs contains g and all g' directly or indirectly imported from g that have the same kind as g
   * if onlySameKind is false; then gs contains g and all g' directly or indirectly imported from g regardless of their kind
   */
  def terminologyGraphImportClosure[Omf <: OMF, TG <: Omf#ModelTerminologyGraph](
    g: TG,
    onlySameKind: Boolean = true )( implicit ops: OMFOps[Omf] ): Set[Omf#ModelTerminologyGraph] = {

    import core.TerminologyKind._
    import ops._

    def getImportedTerminologyGraphs( tbox: Omf#ModelTerminologyGraph ): Set[Omf#ModelTerminologyGraph] = {
      val ( _, _, kind: TerminologyKind, imports: Iterable[Omf#ModelTerminologyGraph], _, _, _, _, _, _, _, _, _, _ ) = fromTerminologyGraph( tbox )
      imports.filter( !onlySameKind || kind == getTerminologyGraphKind( _ ) ).toSet
    }

    OMFOps.closure[Omf#ModelTerminologyGraph, Omf#ModelTerminologyGraph]( g, getImportedTerminologyGraphs ) + g
  }

  /**
   * Aggregates all entities defined in terminology graphs
   * @param tboxes: a set of terminology graphs
   * @return a 3-tuple of the aspects, concepts and relationships entities defined in the graphs:
   */
  def allEntities[Omf <: OMF](
    tboxes: Set[Omf#ModelTerminologyGraph] )( implicit ops: OMFOps[Omf] ): ( Set[Omf#ModelEntityAspect], Set[Omf#ModelEntityConcept], Set[Omf#ModelEntityReifiedRelationship] ) = {

    import ops._

    val entities0 = ( Set[Omf#ModelEntityAspect](), Set[Omf#ModelEntityConcept](), Set[Omf#ModelEntityReifiedRelationship]() )
    val entitiesN =
      ( entities0 /: ( for { tbox <- tboxes } yield {
        val ( _, _, _, _,
          aspects: Iterable[Omf#ModelEntityAspect],
          concepts: Iterable[Omf#ModelEntityConcept],
          relations: Iterable[Omf#ModelEntityReifiedRelationship],
          _, _, _, _, _, _, _ ) =
          fromTerminologyGraph( tbox )
        ( aspects.toSet, concepts.toSet, relations.toSet )
      } ) ) { case ( ( ai, ci, ri ), ( aj, cj, rj ) ) => ( ai ++ aj, ci ++ cj, ri ++ rj ) }

    entitiesN
  }

}