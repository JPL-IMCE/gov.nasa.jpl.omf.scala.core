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

import scala.{Boolean,Int,Ordering}
import scala.collection.immutable.{List,Nil,Set}

sealed trait DescriptionKind { val index: Int }

case object DescriptionFinalKind extends DescriptionKind {
  override val index = 1
}

case object DescriptionPartialKind extends DescriptionKind {
  override val index = 2
}

/**
  * DescriptionKind indicates whether the semantics of the description is partial or complete.
  */
object DescriptionKind {

  import DescriptionKindHelper._

  implicit def ordering: Ordering[DescriptionKind] =
    new Ordering[DescriptionKind] {
      def compare(x: DescriptionKind, y: DescriptionKind): Int
      = if (x.index < y.index) -1
      else if (x.index == y.index) 0
      else 1
    }

  /**
    * isFinal indicates that the semantics of a DescriptionBox is a complete specification
    * about classification, relationships and values of individuals.
    * A final description box cannot be refined.
    */
  val isFinal = DescriptionFinalKind

  /**
    * isPartial indicates that the semantics of a DescriptionBox is a partial specification subject to refinement.
    */
  val isPartial = DescriptionPartialKind

  val values: Set[DescriptionKind] = Values

  def isFinalKind(k: DescriptionKind )
  : Boolean
  = k match {
    case DescriptionKind.`isFinal` => true
    case _ => false
  }

  def isPartialKind(k: DescriptionKind )
  : Boolean
  = k match {
    case DescriptionKind.`isPartial` => true
    case _ => false
  }

}

object DescriptionKindHelper {

  import shapeless.{:+:,CNil,Coproduct,Generic,Witness}

  // Infrastructure for the above. Original version due to Travis Brown,
  //
  //   http://stackoverflow.com/questions/25838411
  //
  object Values {
    implicit def conv[T](self: this.type)(implicit v: MkValues[T]): Set[T] = Values[T]

    def apply[T](implicit v: MkValues[T]): Set[T] = v.values.toSet

    trait MkValues[T] {
      def values: List[T]
    }

    object MkValues {
      implicit def values[T, Repr <: Coproduct]
      (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkValues[T] =
        new MkValues[T] { def values = v.values }

      trait Aux[T, Repr] {
        def values: List[T]
      }

      object Aux {
        implicit def cnilAux[A]: Aux[A, CNil] =
          new Aux[A, CNil] { def values = Nil }

        implicit def cconsAux[T, L <: T, R <: Coproduct]
        (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
          new Aux[T, L :+: R] { def values = l.value :: r.values }
      }
    }
  }

}