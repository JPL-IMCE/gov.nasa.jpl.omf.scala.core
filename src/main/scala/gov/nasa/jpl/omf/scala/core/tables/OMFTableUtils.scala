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

package gov.nasa.jpl.omf.scala.core.tables

import scala.collection.immutable._
import scala.Predef.ArrowAssoc

object OMFTableUtils {

  def mergeMapOfSeq[K, V]
  (m1: Map[K, Seq[V]],
   m2: Map[K, Seq[V]])
  : Map[K, Seq[V]]
  = (m1.keySet ++ m2.keySet)
    .map { k =>
      val v1 = m1.getOrElse(k, Seq.empty)
      val v2 = m2.getOrElse(k, Seq.empty)
      k -> (v1 ++ v2)
    }
    .toMap

  def mergeMapOfMapOfSeq[K1, K2, V]
  (mms1: Map[K1, Map[K2, Seq[V]]],
   mms2: Map[K1, Map[K2, Seq[V]]])
  : Map[K1, Map[K2, Seq[V]]]
  = (mms1.keySet ++ mms2.keySet)
    .map { k =>
      val kv1 = mms1.getOrElse(k, Map.empty)
      val kv2 = mms2.getOrElse(k, Map.empty)
      val k2v = mergeMapOfSeq(kv1, kv2)
      k -> k2v
    }
    .toMap


}
