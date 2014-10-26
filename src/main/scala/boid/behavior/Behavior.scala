package boid.behavior

import boid._

/**
 * Created by markus on 25/10/2014.
 */
object Behavior {
  implicit val maxTurnAngle = math.Pi / 8
  implicit val maxSightAngle = 2 * math.Pi / 3

//  def turn[P <: Position[P]](maxTurnAngle: Double, from: Direction[P])
//                            (to: Direction[P]): Direction[P] = {
//    val angle = from.angleWith(to)
//    if (math.abs(angle) <= maxTurnAngle) to
//    else from.turn(if (angle < 0) -maxTurnAngle else maxTurnAngle)
//    //    to
//  }
//
//  def inSight[P <: Position[P]](maxSightAngle: Double, ownDir: Direction[P])
//                               (bogey: Bogey[P]): Boolean = {
//    val angle = ownDir.angleWith(bogey.direction)
//    math.abs(angle) <= maxSightAngle
//  }
//
//  def closest[P <: Position[P]](lookingAt: Direction[P],
//                                bogeys: Seq[Bogey[P]])
//                               (implicit maxSightAngle: Double): (Bogey[P], Float) = {
//    val inSight0 = inSight(maxSightAngle, lookingAt)(_)
//    bogeys
//      .foldLeft((bogeys.head, Float.MaxValue)) { case ((closest, minDist), bogey) =>
//      if (bogey.distance < minDist && inSight0(bogey)) (bogey, bogey.distance)
//      else (closest, minDist)
//    }
//  }
}

trait Behavior[P <: Position[P]] {
  def react(entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P]
}
