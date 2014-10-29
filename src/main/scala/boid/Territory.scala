package boid

/**
 * Created by markus on 25/10/2014.
 */
// as in map vs. territory. this is the exact content of the world
trait Territory[P <: Position[P]] {
  def add(a: MovingEntity[P], pos: P): Territory[P]
  def remove(a: MovingEntity[P]): Territory[P]

  def withLimits(worldsEnd: P): Territory[P]

  def boids: Map[Boid[P], P]
  def entities: Map[MovingEntity[P], P]
  def positionOf(a: MovingEntity[P]): Option[P]
  def nearby(pos: P, radius: Float): Seq[Bogey[P]]

  def rndPosition(): P
  def rndVelocity(speed: Float): Velocity[P]
}