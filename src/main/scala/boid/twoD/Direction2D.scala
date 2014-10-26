package boid.twoD

import boid.Direction

/**
 * Created by markus on 25/10/2014.
 */
object Direction2D {
  def apply(x: Float, y: Float): Direction[Position2D] = {
    val norm = math.sqrt(x*x+y*y).toFloat
    if (norm == 0) default
    else new Direction2D(x/norm, y/norm)
  }

  val default: Direction[Position2D] = new Direction2D(0,1)
}

private class Direction2D(normalizedX: Float,
                          normalizedY: Float)
                   extends Direction[Position2D] {
  def fromPosition(position: Position2D, speed: Float): Position2D = {
    Position2D(position.x + normalizedX*speed, position.y + normalizedY*speed)
  }

  def opposite: Direction[Position2D] = {
    new Direction2D(-normalizedX, -normalizedY)
  }

  override def toString = s"Direction2D($normalizedX, $normalizedY)"
}
