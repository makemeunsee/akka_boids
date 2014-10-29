package boid

import akka.actor._
import scala.concurrent.duration._

/**
 * Created by markus on 25/10/2014.
 */
object World {
  val tickRate = 15.millis
  // 0 for as fast as possible
  // see config for akka internal tick rate, as it makes no sense to have a movementInterval lower than that
  val movementInterval = 4.millis

  private object InternalTick
  object Start
  object Stop

  case class AddBoid(boid: ActorRef, color: Int)
  case class RemoveBoid(boid: ActorRef)

  sealed trait HunterMsg
  case class AddHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg
  case class MoveHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg
  case class RemoveHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg

  case class WorldEndUpdate[P <: Position[P]](newWorldsEnd: P)

  case class SendBogeys(to: ActorRef)

  case class Flock[P <: Position[P]](boids: Map[Boid[P], P])

  val sightRadius = 20

  val hunterBaseID = 1000000000l
}

import World._

class World[P <: Position[P]](emptyTerritory: Territory[P],
                              ui: ActorRef) extends Actor {

  import context.dispatcher
  private def newTickingTask(): Cancellable = {
    context.system.scheduler.schedule(tickRate, tickRate, self, InternalTick)
  }

  def stopped: Receive = {
    case Start =>
      context.become(running(newTickingTask(), Map.empty)(emptyTerritory))
      ui ! Start
  }

  override def receive: Receive = stopped

  def running(tickingTask: Cancellable,
              boids: Map[ActorRef, (Boid[P], Long)])
             (implicit territory: Territory[P]): Receive = {

    case Stop =>
      tickingTask.cancel()
      context.become(stopped)
      // TODO put shutdown in manager actor later
      boids.keys foreach { _ !  Kill }
      context.system.stop(self)
      context.system.shutdown()

    case InternalTick =>
      ui ! Flock(territory.boids)

    case i: Intention[P] =>
      boids.get(sender) foreach { case (boid, lastMove) =>
        val now = System.currentTimeMillis
        if (now + movementInterval.toMillis >= lastMove) {
          val (newBoids, newTerritory) = applyIntention(sender, i, boid, boids, now)
          context.become(running(tickingTask, newBoids)
                        (newTerritory))
          // schedule bogeys to sender after movementInterval
          context.system.scheduler.scheduleOnce(movementInterval,
            self,
            SendBogeys(sender))
        }
      }

    case SendBogeys(ref) =>
      boids.get(ref) foreach { case (boid, pos) =>
        ref ! BogeysMsg(boid, around(boid))
      }

    case ah: AddHunter[P] =>
      context.become(running(tickingTask, boids)
        (territory.add(ah.hunter, ah.hunter.position)))

    case rh: RemoveHunter[P] =>
      context.become(running(tickingTask, boids)
        (territory.remove(rh.hunter)))

    case mh: MoveHunter[P] =>
      val h = mh.hunter
      context.become(running(tickingTask, boids)
        (territory.remove(h).add(h, h.position)))

    case AddBoid(boidActor, color) =>
      val newBoid = Boid(territory.rndVelocity(Boid.defaultSpeed), color)
      context.become(running(tickingTask,
        boids + ((boidActor, (newBoid, 0l))))
        (territory.add(newBoid, territory.rndPosition())))
      boidActor ! BogeysMsg(newBoid, Seq.empty)

    case RemoveBoid(boidActor) =>
      val boid_? = boids.get(boidActor)
      boid_? foreach { case (boid, _) =>
        context.become(running(tickingTask,
          boids - boidActor)
          (territory.remove(boid)))
      }

    case weu: WorldEndUpdate[P] =>
      context.become(running(tickingTask,
        boids)
        (territory.withLimits(weu.newWorldsEnd)))
  }

  private def applyIntention(from: ActorRef,
                             i: Intention[P],
                             boid: Boid[P],
                             boids: Map[ActorRef, (Boid[P], Long)],
                             now: Long)
                            (implicit territory: Territory[P]): (Map[ActorRef, (Boid[P], Long)], Territory[P]) = {
    // update territory
    val (newTerritory, newBoid_?) = applyIntention(boid, i)
    // update state
    val newBoids = newBoid_?
      .map { b =>
      boids + ((from, (b, now)))
    }
    .getOrElse(boids)

    // return new state
    (newBoids, newTerritory)
  }

  private def applyIntention(boid: Boid[P],
                             i: Intention[P])
                            (implicit territory: Territory[P]): (Territory[P], Option[Boid[P]]) = {
    territory.positionOf(boid) match {
      case None =>
        (territory, None)

      case Some(oldPos) =>
        val newBoid = new Boid(i.velocity, boid.color, boid.id)
        (territory.add(newBoid, i.velocity.from(oldPos)), Some(newBoid))
    }
  }

  private def around(boid: MovingEntity[P])(implicit territory: Territory[P]): Seq[Bogey[P]] = {
    territory
      .positionOf(boid)
      .map(p => territory
                  .nearby(p, sightRadius)
                  .filter(b => b.id != boid.id))
      .getOrElse(Seq.empty)
  }
}
