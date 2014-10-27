package boid.twoD

import boid._

import scala.annotation.tailrec

/**
 * Created by markus on 25/10/2014.
 */
case class Territory2D(width: Int,
                       height: Int,
                       entities: Map[MovingEntity[Position2D], Position2D] = Map.empty)
      extends Territory[Position2D] {

  if (width <= 0) throw new Error(s"width <= 0: $width")
  if (height <= 0) throw new Error(s"height <= 0: $width")

  @tailrec
  private def inbound(pos: Position2D): Position2D = {
    if (pos.x < 0) inbound(Position2D(pos.x + width, pos.y))
    else if (pos.x >= width) inbound(Position2D(pos.x - width, pos.y))
    else if (pos.y < 0) inbound(Position2D(pos.x, pos.y + height))
    else if (pos.y >= height) inbound(Position2D(pos.x, pos.y - height))
    else pos
  }

  def add(a: MovingEntity[Position2D], pos: Position2D): Territory[Position2D] = {
    new Territory2D(width, height, entities + ((a, inbound(pos))))
  }

  def remove(a: MovingEntity[Position2D]): Territory[Position2D] = {
    new Territory2D(width, height, entities - a)
  }

  def withLimits(worldsEnd: Position2D): Territory2D = {
    if (worldsEnd.x <= 0 || worldsEnd.y <= 0) throw new Error("wrong world size")
    copy(width = worldsEnd.x.toInt, height = worldsEnd.y.toInt)
  }

  def boids: Map[Boid[Position2D], Position2D] = {
    entities
      .filterKeys(_.isInstanceOf[Boid[Position2D]])
      .map { case (e, p) =>
        (e.asInstanceOf[Boid[Position2D]], p)
      }
  }

  def positionOf(a: MovingEntity[Position2D]): Option[Position2D] = {
    entities.get(a)
  }

  def nearby(from: Position2D, radius: Float): Seq[Bogey[Position2D]] = {
    // TODO circular distancing
    entities
      .map { case (e, p) =>
        new Bogey[Position2D](e.id,
          e.allegiance,
          e.velocity,
          from.distanceTo(p),
          from.directionTo(p)
        )
    }
    .toSeq
  }

  def rndPosition(): Position2D = {
    Position2D((math.random * width).toFloat, (math.random * height).toFloat)
  }

  def rndVelocity(speed: Float): Velocity[Position2D] = {
    Direction2D(math.random.toFloat * 10 - 5, math.random.toFloat * 10 - 5).withSpeed(speed)
  }
}
