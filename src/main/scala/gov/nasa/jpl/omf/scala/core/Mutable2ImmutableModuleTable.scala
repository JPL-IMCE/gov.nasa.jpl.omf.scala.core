package gov.nasa.jpl.omf.scala.core

import java.util.UUID

import gov.nasa.jpl.omf.scala.core.OMFError.Throwables

import scala.collection.immutable._
import scala.{Boolean, Int, Option, None, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case class Mutable2ImmutableModuleTable[omf <: OMF]
( pairs: Seq[(omf#MutableModule, omf#ImmutableModule)] = Seq.empty[(omf#MutableModule, omf#ImmutableModule)],
  ms: Map[UUID, omf#MutableModule] = Map.empty[UUID, omf#MutableModule],
  is: Map[UUID, omf#ImmutableModule] = Map.empty[UUID, omf#ImmutableModule]) {

  def keys: Seq[omf#MutableModule] = pairs.map(_._1)

  def values: Seq[omf#ImmutableModule] = pairs.map(_._2)

  def containsKey
  (mm: omf#MutableModule)
  (implicit ops: OMFOps[omf])
  : Boolean
  = ms.contains(ops.getModuleUUID(mm))

  def containsValue
  (im: omf#ImmutableModule)
  (implicit ops: OMFOps[omf])
  : Boolean
  = is.contains(ops.getModuleUUID(im))

  def lookupValue
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableModule]
  = values.find(ops.getModuleIRI(_) == iri)

  def size: Int = pairs.size

  def `:+`
  (pair: (omf#MutableModule, omf#ImmutableModule))
  (implicit ops: OMFOps[omf])
  : Throwables \/ Mutable2ImmutableModuleTable[omf]
  = pairs.find(_._1 == pair._1) match {
    case Some((_, im)) =>
      if (im == pair._2)
        this.right
      else
        Set[java.lang.Throwable](
          OMFError.omfError(
            s"Mutable2ImmutableModuleTable: key conflict: ${pair._1} is already mapped to a different value"
          )).left
    case None =>
      val uuid = ops.getModuleUUID(pair._1)
      copy(pairs = pairs :+ pair, ms = ms + (uuid -> pair._1), is = is + (uuid -> pair._2)).right
  }

  def get(m: omf#MutableModule): Option[omf#ImmutableModule] = pairs.find(_._1 == m).map(_._2)

}

object Mutable2ImmutableModuleTable {

  def empty[omf <: OMF]
  : Mutable2ImmutableModuleTable[omf]
  = Mutable2ImmutableModuleTable[omf]()
}