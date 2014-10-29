package boid.twoD

import boid._

import scala.annotation.tailrec

/**
 * Created by markus on 25/10/2014.
 */
object Territory2D {

  val cellSpan = World.sightRadius
}

import Territory2D._

case class Territory2D(width: Int,
                       height: Int,
                       entities: Map[MovingEntity[Position2D], Position2D] = Map.empty,
                       grid: Map[(Int, Int), Set[MovingEntity[Position2D]]] = Map.empty)
      extends Territory[Position2D] {

  if (width <= 0) throw new Error(s"width <= 0: $width")
  if (height <= 0) throw new Error(s"height <= 0: $width")

  val cellColumnCount = (width-1) / cellSpan
  val cellRowCount = (height-1) / cellSpan

  private def cellCoords(p: Position2D): (Int, Int) = {
    (p.x.toInt / cellSpan, p.y.toInt / cellSpan)
  }

  @tailrec
  private def inbound(pos: Position2D): Position2D = {
    if (pos.x < 0) inbound(Position2D(pos.x + width, pos.y))
    else if (pos.x >= width) inbound(Position2D(pos.x - width, pos.y))
    else if (pos.y < 0) inbound(Position2D(pos.x, pos.y + height))
    else if (pos.y >= height) inbound(Position2D(pos.x, pos.y - height))
    else pos
  }

  def add(a: MovingEntity[Position2D], pos: Position2D): Territory[Position2D] = {

    val in = inbound(pos)

    val (i,j) = cellCoords(in)

    val newGrid = positionOf(a)
    .map { oldP =>

      // if there was an old position for this entity
      val (oldI, oldJ) = cellCoords(oldP)
      // check it's in a different grid cell
      if (oldI != i || oldJ != j) {
        val emptied = grid((oldI, oldJ)) - a
        val filled = grid.get((i,j)).map { set =>
          set - a + a
        }
        .getOrElse(Set(a))

        grid + (((i,j), filled)) + (((oldI,oldJ), emptied))
      } else {
        grid
      }
    }
    .getOrElse {
      // add the entity to its cell if there was no old position for it
      val filled = grid.get((i,j)).map { set =>
        set + a
      }
      .getOrElse(Set(a))
      grid + (((i,j), filled))
    }

    new Territory2D(width, height, entities - a + ((a, in)), newGrid)
  }

  def remove(a: MovingEntity[Position2D]): Territory[Position2D] = {
    val newGrid = positionOf(a)
    .map { oldP =>
      val (i,j) = cellCoords(oldP)
      val emptied = grid((i,j)) - a
      grid + (((i,j), emptied))
    }
    .getOrElse(grid)

    new Territory2D(width, height, entities - a, newGrid)
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

  def properColumn(i: Int) = i >= 0 && i < cellColumnCount
  def properRow(j: Int) = j >= 0 && j < cellRowCount

  def nearby(from: Position2D, radius: Float): Seq[Bogey[Position2D]] = {
    val (i,j) = cellCoords(inbound(from))
    (-1 to 1).map(_ + i).filter(properColumn).flatMap { ii =>
    (-1 to 1).map(_ + j).filter(properRow).flatMap { jj =>
      grid.getOrElse((ii,jj), Set.empty)
    } }
    .map { e =>
      val p = positionOf(e).get
      new Bogey[Position2D](e.id,
        e.allegiance,
        e.velocity,
        from.distanceTo(p),
        from.directionTo(p))
    }
  }

  def rndPosition(): Position2D = {
    Position2D((math.random * width).toFloat, (math.random * height).toFloat)
    // Position2D(0,0)
  }

  def rndVelocity(speed: Float): Velocity[Position2D] = {
    Direction2D(math.random.toFloat * 10 - 5, math.random.toFloat * 10 - 5).withSpeed(speed)
    // Velocity2D(2,0)
  }
}
