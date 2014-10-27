package boid.behavior

import boid._

/**
 * Created by markus on 25/10/2014.
 */
trait Behavior[P <: Position[P]] {
  def react(entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P]
  def color: Int
}
