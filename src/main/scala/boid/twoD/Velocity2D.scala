package boid.twoD

import boid.{Direction, Velocity}

/**
 * Created by markus on 26/10/2014.
 */
case class Velocity2D(x: Float, y: Float) extends Velocity[Position2D] {
  def opposite: Velocity2D = {
    Velocity2D(-x, -y)
  }

  def +(otherVelocity: Velocity[Position2D]): Velocity2D = {
    Velocity2D(x + otherVelocity.components(0), y + otherVelocity.components(1))
  }

  def components: Seq[Float] = Seq(x, y)

  def from(p: Position2D): Position2D = Position2D(p.x + x, p.y + y)

  def toDirection: Direction[Position2D] = Direction2D(x ,y)

  lazy val speed: Float = math.sqrt(x*x+y*y).toFloat

  def withSpeed(newSpeed: Float): Velocity2D = {
    if (speed == 0) {
      val v = math.sqrt(math.abs(newSpeed)).toFloat
      Velocity2D(v, v)
    } else {
      val f = newSpeed / speed
      Velocity2D(x * f, y * f)
    }
  }

  def *(by: Float): Velocity2D = {
    Velocity2D(x * by, y * by)
  }

  def /(by: Float): Velocity2D = {
    Velocity2D(x / by, y / by)
  }

  def dot(by: Velocity[Position2D]): Float = {
    x * by.components(0) + y * by.components(1)
  }
}
