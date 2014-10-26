package boid.twoD

import boid.{Direction, Position}

/**
 * Created by markus on 25/10/2014.
 */
case class Position2D(x: Float, y: Float) extends Position[Position2D] {
  def distanceTo(otherP: Position2D): Float = {
    math.sqrt((x-otherP.x)*(x-otherP.x)+(y-otherP.y)*(y-otherP.y)).toFloat
  }

  def directionTo(to: Position2D): Direction[Position2D] = {
    Direction2D(to.x - x, to.y - y)
  }
}
