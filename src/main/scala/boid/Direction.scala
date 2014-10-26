package boid

/**
 * Created by markus on 25/10/2014.
 */
trait Direction[P <: Position[P]] {
  def fromPosition(position: P, speed: Float): P
  def opposite: Direction[P]
}
