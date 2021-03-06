package boid

import akka.actor.Actor
import boid.behavior.Behavior

/**
 * Created by markus on 25/10/2014.
 */
case class BogeysMsg[P <: Position[P]](boid: MovingEntity[P], bogeys: Seq[Bogey[P]])
object Identify
case class Identity(behaviorClass: String)

class BoidActor[P <: Position[P]](behavior: Behavior) extends Actor {

  override def receive: Receive = {
    case msg: BogeysMsg[P] =>
      sender ! behavior.react(msg.boid, msg.bogeys)

    case Identify =>
      sender ! Identity(behavior.getClass.getCanonicalName)
  }
}
