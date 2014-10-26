package boid

/**
 * Created by markus on 25/10/2014.
 */
abstract case class MovingEntity[P <: Position[P]](id: Long, allegiance: String) {
  def movingVector: Direction[P]
}
