package boid.behavior

import boid._

/**
 * Created by markus on 26/10/2014.
 */
object ScaredBehavior {
  val color = 0x4444ff
}

class ScaredBehavior extends Behavior {
  val avoidDistance = World.sightRadius

  val speed = 5

  def color = ScaredBehavior.color

  def react[P <: Position[P]](entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P] = {
    val inRange = bogeys
      .filter(b => b.distance <= avoidDistance && (b.direction dot entity.velocity.toDirection) > -0.72f)

    val v = inRange.foldLeft(entity.velocity.withSpeed(speed)) { case (d, bogey) =>
      if (bogey.allegiance == Boid.boidFaction)
        d + bogey.direction.opposite
      else
        d + bogey.direction.opposite * 2
    }
    Intention(v)
  }
}
