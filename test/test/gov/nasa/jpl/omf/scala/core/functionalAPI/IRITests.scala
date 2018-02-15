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

package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core._

import scala.Predef._
import scala.{Any,Boolean,Option,None,Some,Tuple2, Unit}
import scala.collection.immutable.{Map,Seq}
import scalaz._, Scalaz._, Kleisli._
import org.scalatest._

abstract class IRITests[omf <: OMF[omf]]()( implicit ops: OMFOps[omf] )
extends WordSpec with Matchers {

  import ops._

  type Result[A] = Set[java.lang.Throwable] \/ A
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