package boid

/**
 * Created by markus on 25/10/2014.
 */
// as in map vs. territory. this is the exact content of the world
trait Territory[P <: Position[P]] {
  def add(a: MovingEntity[P], pos: P): Territory[P]
  def remove(a: MovingEntity[P]): Territory[P]
  def move(a: MovingEntity[P], to: P): Territory[P] = remove(a).add(a, to)

  def entities: Map[MovingEntity[P], P]
  def positionOf(a: MovingEntity[P]): Option[P]
  def nearby(pos: P, radius: Float): Seq[Bogey[P]]

  def rndPosition(): P
  def rndDirection(): Direction[P]
}