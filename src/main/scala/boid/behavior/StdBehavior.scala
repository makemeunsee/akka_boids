package boid.behavior

import boid._

/**
 * Created by markus on 25/10/2014.
 */
object StdBehavior {
  val color = 0x00ff00
}

class StdBehavior extends Behavior {
  val speed = 4

  val maxSteering = 0.01f
  val alignSpeed = 0.5f

  val radius = World.sightRadius / 2

  def color = StdBehavior.color

  def react[P <: Position[P]](entity: MovingEntity[P], bogeys: Seq[Bogey[P]]): Intention[P] = {
    val v0 = entity.velocity.withSpeed(0)

    val (friends, foes) = bogeys
      .filter(b => (b.allegiance == Boid.boidFaction || b.allegiance == Hunter.hunterFaction)
                   && b.distance <= 2*radius
                   && b.distance > 0)
      .partition(b => b.allegiance == Boid.boidFaction)

    // strongly avoid foes
    val avoid = foes.foldLeft(v0) { case (d, bogey) =>
      d + bogey.direction.opposite * 10 / bogey.distance
    }

    // cohesion
    val friendsCount = friends.size
    val (dirAvg, velAvg)= friends.foldLeft((v0, v0)) { case ((d, v), bogey) =>
      (d + bogey.direction / friendsCount, v + bogey.velocity / friendsCount)
    }
    val cohesionVec = if (friendsCount == 0) v0 else dirAvg.withSpeed(speed) + entity.velocity.opposite
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
    val closeFriends = friends.filter(b => b.distance <= radius)
    val closeFriendsCount = closeFriends.size
    val sep = closeFriends.foldLeft(v0) { case (v, f) =>
      v + f.direction.opposite / f.distance / math.sqrt(closeFriendsCount).toFloat
    }

    val all = entity.velocity + sep + align + steering + avoid
    val intent = if (all.speed > speed)
                   all.withSpeed(speed)
                 else
                   all

    Intention(intent)
  }
}
