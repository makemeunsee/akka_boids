package boid

/**
 * Created by markus on 25/10/2014.
 */
case class Intention[P <: Position[P]](velocity: Velocity[P])
