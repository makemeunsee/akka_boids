package boid.behavior

import boid._

/**
 * Created by markus on 26/10/2014.
 */
object ScaredBehavior extends Behavior {
  val fleeDistance = World.sightRadius
  val avoidDistance = World.sightRadius / 2

  val speed = 5

  val color = 0x4444ff

  def react[P <: Position[P]](entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P] = {
    val inRange = bogeys
      .filter(b => b.allegiance == Boid.boidFaction && b.distance <= avoidDistance
                || b.allegiance == Hunter.hunterFaction && b.distance <= fleeDistance)

    val count = inRange.size
    val v = inRange.foldLeft(entity.velocity.withSpeed(speed)) { case (d, bogey) =>
      d + bogey.direction.opposite / count
    }
    Intention(v)
  }
}
