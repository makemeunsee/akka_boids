package boid

/**
 * Created by markus on 26/10/2014.
 */
object Hunter {
  val hunterFaction = "osprey"
}

import Hunter._

class Hunter[P <: Position[P]](id: Long,
                               val velocity: Velocity[P],
                               val position: P)
      extends MovingEntity[P](id, hunterFaction)