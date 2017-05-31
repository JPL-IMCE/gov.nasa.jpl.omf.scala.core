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

import scala.Predef.{identity,String}

/**
  * OMLString defines multiple tag types on String:
  * - LocalName
  * - LexicalValue
  * - LexicalNumber
  * - LexicalTime
  * - Pattern
  * - LangRange
  * - AbbrevIRI
  * - NamespacePrefix
  *
  * Since these String-tagged types are disjoint from each other,
  * there is a compile-time guarantee that a particular tagged-String value can't be misused
  * as a legimate String of a different tag.
  * (e.g., an AbbrevIRI String value can't be used as an argument for a function accepting a Pattern String)
  *
  * @see https://code.launchpad.net/~scompall/+junk/high-cost-of-anyval-subclasses
  * @see https://failex.blogspot.com/2017/04/the-high-cost-of-anyval-subclasses.html
  */
object OMLString {

  sealed abstract class OMLStringImpl {
    type T <: String

    def apply(s: String): T

    def unwrap(lbl: T): String

    /**
     * Convert an F[_] structure of Strings to an F[_] structure of T in constant time.
     */
    def subst[F[_]](fs: F[String]): F[T]

    // In S. Compall's article, the code is written with the Scala kind-projector compiler plugin:
    //    Lambda[x => F[x] => F[String]]
    // Without this plugin, we have to write instead:
    //    ({type U[x] = F[x] => F[String]})#U
    /**
     * Convert an F[_] structure of T to an F[String] structure in constant time.
     */
    def untag[F[_]](f: F[T]): F[String] = subst[({type U[x] = F[x] => F[String]})#U](identity)(f)
  }

  /**
   * OMLCommon#LocalName
   */
  val LocalName : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type LocalName = LocalName.T

  /**
   * OMLCommon#LexicalValue
   */
  val LexicalValue : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type LexicalValue = LexicalValue.T

  /**
    * OMLCommon#LexicalNumber
    */
  val LexicalNumber : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type LexicalNumber = LexicalNumber.T

  /**
    * OMLCommon#LexicalTime
    */
  val LexicalTime : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type LexicalTime = LexicalTime.T

  /**
    * OMLCommon#Pattern
    */
  val Pattern : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type Pattern = Pattern.T

  /**
    * OMLCommon#LangRange
    */
  val LangRange : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type LangRange = LangRange.T

  /**
    * OMLCommon#AbbrevIRI
    */
  val AbbrevIRI : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type AbbrevIRI = AbbrevIRI.T

  /**
    * OMLCommon#NamespacePrefix
    */
  val NamespacePrefix : OMLStringImpl = new OMLStringImpl {

    override type T = String

    override def apply(s: String): T = s

    override def unwrap(lbl: T): String = lbl

    override def subst[F[_]](fs: F[String]): F[T] = fs

  }

  type NamespacePrefix = NamespacePrefix.T

}
