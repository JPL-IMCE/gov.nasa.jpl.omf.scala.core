package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class ReifiedRelationshipInstanceSignature[omf <: OMF]
( uuid: UUID,
  reifiedRelationshipClassifier: omf#ReifiedRelationship )
