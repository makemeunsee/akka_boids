package boid.behavior

/**
 * Created by markus on 25/10/2014.
 */
object StdBehavior {
  val speed = 2

  val maxSteering = 0.01f
  val alignSpeed = 0.5f

  val radius = 80
}

import boid.behavior.StdBehavior._
import boid.{Bogey, Boid, Hunter, Intention, MovingEntity, Position}

class StdBehavior[P <: Position[P]] extends Behavior[P] {

  def react(entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P] = {
    val v0 = entity.velocity.withSpeed(0)

    val (friends, foes) = bogeys
      .filter(b => b.allegiance == Boid.boidFaction && b.distance <= radius && b.distance > 0
                || b.allegiance == Hunter.hunterFaction && b.distance <= radius * 2 && b.distance > 0)
      .partition(b => b.allegiance == Boid.boidFaction)

    // avoid foes
    val foesCount = foes.size
    val avoid = foes.foldLeft(v0) { case (d, bogey) =>
      d + bogey.direction.opposite * 10 / bogey.distance / foesCount
    }

    // cohesion
    val friendsCount = friends.size
    val (dirAvg, velAvg)= friends.foldLeft((v0, v0)) { case ((d, v), bogey) =>
      (d + bogey.direction / friendsCount, v + bogey.velocity / friendsCount)
    }
    val cohesionVec = dirAvg.withSpeed(speed) + entity.velocity.opposite
    val steering = if (cohesionVec.speed > maxSteering)
                     cohesionVec.withSpeed(maxSteering)
                   else
                     cohesionVec

    // align
    val align = if (velAvg.speed > alignSpeed)
                  velAvg.withSpeed(alignSpeed)
                else
                  velAvg

    // separate
    val closeFriends = friends.filter(b => b.distance <= radius / 2f)
    val closeFriendsCount = closeFriends.size
    val sep = closeFriends.foldLeft(v0) { case (v, f) =>
      v + f.direction.opposite / f.distance / closeFriendsCount
    }

    val all = entity.velocity + sep + align + steering + avoid
    val intent = if (all.speed > speed)
                   all.withSpeed(speed)
                 else
                   all

    Intention(intent)
  }
}