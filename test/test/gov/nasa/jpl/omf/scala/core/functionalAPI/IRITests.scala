/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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
package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core._

import scala.Predef._
import scala.{Any,Boolean,Option,None,Some,Tuple2, Unit}
import scala.collection.immutable.{Map,Seq}
import scala.language.implicitConversions
import scala.language.postfixOps
import scalaz._, Scalaz._, Kleisli._
import org.scalatest._

abstract class IRITests[omf <: OMF]()( implicit ops: OMFOps[omf] )
extends WordSpec with Matchers {

  import ops._

  type Result[A] = NonEmptyList[java.lang.Throwable] \/ A
  type ResultTo[A, B] = Kleisli[Result, A, B]

  def ResultTo[In, Out](fn: In => Result[Out]): ResultTo[In, Out] =
    Kleisli[Result, In, Out](fn)

  implicit val resultBinding = new Bind[Result] {

    def map[A, B](fa: Result[A])(f: A => B): Result[B] = {
      fa.map(f)
    }

    def bind[A, B](fa: Result[A])(f: A => Result[B]): Result[B] = {
      fa.flatMap(f)
    }

  }

  type UnitToStringSeqF = Unit => Result[Seq[String]]
  type StringsToIRIsF = Seq[String] => Result[Seq[omf#IRI]]

  def stringsToIRIs: StringsToIRIsF = (iris: Seq[String]) => {

    val r0: Result[Seq[omf#IRI]] = Seq().right
    val rN: Result[Seq[omf#IRI]] = (r0 /: iris) {
      (ri, iri) =>
        ri +++ makeIRI(iri).map(Seq(_))
    }

    rN
  }

  implicit def omfIRISeqSemigroup: Semigroup[Seq[omf#IRI]] =
    Semigroup.instance(_ ++ _)

  "IRI backbone tests" when {
    "is backbone" in {

      val iri1: UnitToStringSeqF =
        (_: Unit) => Seq(
          "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/foundation/mission/mission#Thing"
          ).right


      val k1 = kleisli[Result, Unit, Seq[String]](iri1)

      val k2 = kleisli[Result, Seq[String], Seq[omf#IRI]](stringsToIRIs)

      val k12 = k1 >=> k2

      val result = k12(())

      result.isRight should be(true)
    }
    
    "to backbone" in {
      val iri2bs = Map( 
           "http://imce.jpl.nasa.gov/foundation/mission/mission" -> "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/foundation/mission/mission",
           "http://imce.jpl.nasa.gov/foundation/mission/mission#" -> "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/foundation/mission/mission#",
           "http://imce.jpl.nasa.gov/foundation/mission/mission#Thing" -> "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/foundation/mission/mission#Thing",
           "http://www.omg.org/spec/UML/20110701/UML" -> "http://imce.jpl.nasa.gov/backbone/www.omg.org/spec/UML/20110701/UML",
           "http://www.omg.org/spec/UML/20110701/UML#" -> "http://imce.jpl.nasa.gov/backbone/www.omg.org/spec/UML/20110701/UML#",
           "http://www.omg.org/spec/UML/20110701/UML#Thing" -> "http://imce.jpl.nasa.gov/backbone/www.omg.org/spec/UML/20110701/UML#Thing" 
          )

      // @todo re-enable this test
//      for {
//        ( i, b ) <- iri2bs
//        iri = makeIRI( i )
//        b_iri = makeIRI( b )
//      } {
//        isBackboneIRI( iri ) should be (false)
//        isBackboneIRI( b_iri ) should be (true)
//        toBackboneIRI( iri ) should equal( b_iri )
//      }
    }
  }
  "IRI construction tests" when {
    "fromIRI(makeIRI(s)) == s" in {
      
      val iris = Seq(
          "http://imce.jpl.nasa.gov/foundation/mission/mission",
          "http://imce.jpl.nasa.gov/foundation/mission/mission#Component",
          "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML",
          "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML#"
          )

      // @todo re-enable this test
//      for { iri <- iris } {
//        fromIRI( makeIRI( iri ) ) should be(iri)
//      }
    }
  }
        
  "IRI query tests" when {
    "split" in {
      
      val iris2splits = Map(
          "http://imce.jpl.nasa.gov/foundation/mission/mission" ->
            Tuple2( "http://imce.jpl.nasa.gov/foundation/mission/mission", None ),
          "http://imce.jpl.nasa.gov/foundation/mission/mission#Component" ->
            Tuple2( "http://imce.jpl.nasa.gov/foundation/mission/mission", Some( "Component") ),
          "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML" ->
            Tuple2( "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML", None ),
          "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML#" ->
            Tuple2( "http://imce.jpl.nasa.gov/backbone/imce.jpl.nasa.gov/www.omg.org/spec/UML/20110701/UML#", None )
          )
          
      implicit val iriSplitEquality = new org.scalactic.Equality[(omf#IRI, Option[String])] {
        
        def areEqual( a: (omf#IRI, Option[String]), b: Any): Boolean = 
          b match {
            case ( bIRI: String, None ) => 
              val sameIRI = fromIRI(a._1) == bIRI 
              val sameFragment = a._2 == None
              sameIRI && sameFragment
            case ( bIRI: String, Some( bFragment ) ) => 
              val sameIRI = fromIRI(a._1) == bIRI 
              val sameFragment = a._2.isDefined && a._2.get == bFragment
              sameIRI && sameFragment
            case _ => false
          }

      }

      // @todo re-enable this test
//      for { ( iri, ( stem, fragment ) ) <- iris2splits } {
//        val actual = splitIRI( makeIRI( iri ) )
//        val expected =  ( stem, fragment )
//        actual should equal( expected )
//      }
    }   
  }

}