package boid

/**
 * Created by markus on 25/10/2014.
 */
class Bogey[P <: Position[P]](id: Long,
            allegiance: String,
            val movingVector: Direction[P],
            val distance: Float,
            val direction: Direction[P])
      extends MovingEntity[P](id, allegiance)
