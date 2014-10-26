package boid

/**
 * Created by markus on 25/10/2014.
 */
object Boid {
  val boidFaction = "boids"

  val defaultSpeed = 2

  val nextId: () => Long = {
    var currentId = -1l
    () => {
      currentId += 1
      currentId
    }
  }

  def apply[P <: Position[P]](velocity: Velocity[P]): Boid[P] = {
    new Boid(velocity)
  }
}

import Boid.boidFaction

class Boid[P <: Position[P]](val velocity: Velocity[P],
                             id: Long = Boid.nextId())
      extends MovingEntity[P](id, boidFaction)
