package boid

/**
 * Created by markus on 25/10/2014.
 */
object StdBehavior {
  val cruisingSpeed = 3
  val fleeingSpeed = 6

  val fleeingDistance = 30
  val pursuingDistance = 30
  val aligningDistance = 20
  val breathingDistance = 10
}

import StdBehavior._

class StdBehavior[P <: Position[P]] extends Behavior[P] {
  def react(entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P] = {
    if (bogeys.isEmpty)
      Intention(entity.movingVector, cruisingSpeed)
    else {
      val (closest, dist) =
        bogeys
          .foldLeft((bogeys.head, Float.MaxValue)) { case ((closest, minDist), bogey) =>
          if (bogey.distance < minDist) (bogey, bogey.distance)
          else (closest, minDist)
        }
      closest.allegiance match {
        case Hunter.hunterFaction if dist > fleeingDistance =>
          Intention(entity.movingVector, cruisingSpeed)

        case Hunter.hunterFaction =>
          Intention(closest.direction.opposite, fleeingSpeed)

        case Boid.boidFaction if dist > pursuingDistance =>
          Intention(entity.movingVector, cruisingSpeed)

        case Boid.boidFaction if dist > aligningDistance =>
          Intention(closest.direction, cruisingSpeed)

        case Boid.boidFaction if dist > breathingDistance =>
          Intention(closest.movingVector, cruisingSpeed)

        case Boid.boidFaction =>
          Intention(closest.direction.opposite, cruisingSpeed)
      }
    }
  }
}
