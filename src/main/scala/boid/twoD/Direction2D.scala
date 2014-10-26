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

  val default: Direction[Position2D] = new Direction2D(1,0)
}

private class Direction2D(normalizedX: Float,
                          normalizedY: Float)
                   extends Velocity2D(normalizedX, normalizedY)
                   with Direction[Position2D] {
//  def angleWith(otherDir: Direction[Position2D]): Double = {
//    math.atan2(otherDir.components(0) - normalizedX, otherDir.components(1) - normalizedY)
//  }
//
//  def turn(angle: Double): Direction[Position2D] = {
//    val (cos, sin) = (math.cos(angle).toFloat, math.sin(angle).toFloat)
//    Direction2D(normalizedX*cos - normalizedY*sin, normalizedX*sin + normalizedY*cos)
//  }

  override def toString = s"Direction2D($normalizedX, $normalizedY)"
}
