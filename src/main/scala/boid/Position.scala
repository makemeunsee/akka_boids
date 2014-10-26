package boid

/**
 * Created by markus on 25/10/2014.
 */
trait Position[P <: Position[P]] {
  def distanceTo(otherPosition: P): Float
  def directionTo(to: P): Direction[P]
}
