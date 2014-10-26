package boid

/**
 * Created by markus on 26/10/2014.
 */
trait Velocity[P <: Position[P]] {
  def +(otherVelocity: Velocity[P]): Velocity[P]
  def from(position: P): P
  def opposite: Velocity[P]
  def components: Seq[Float]
  def toDirection: Direction[P]
  def speed: Float
  def withSpeed(speed: Float): Velocity[P]
  def /(by: Float): Velocity[P]
  def *(by: Float): Velocity[P]
}
