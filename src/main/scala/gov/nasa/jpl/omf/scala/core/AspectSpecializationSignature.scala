package gov.nasa.jpl.omf.scala.core

import java.util.UUID

case class AspectSpecializationSignature[omf <: OMF]
( uuid: UUID,
  sub: omf#Entity,
  sup: omf#Aspect
)
