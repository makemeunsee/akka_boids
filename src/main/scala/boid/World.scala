package boid

import akka.actor._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
 * Created by markus on 25/10/2014.
 */
object World {
  val tickRate = 15.millis
  // 0 for as fast as possible
  // see config for akka internal tick rate, as it makes no sense to have a movementInterval lower than that
  val movementInterval = 10.millis

  val defaultWidth = 1024
  val defaultHeight = 768

  private object InternalTick
  object Start
  object Stop
  object UIFlag
  object RequestSaveInfo
  case class SaveInfo[P <: Position[P]](data: Map[Boid[P], String])

  case class AddBoid[P <: Position[P]](boidActor: ActorRef, color: Int, boidInfo: Option[(Boid[P], P)] = None)
  case class RemoveBoid(boid: ActorRef)

  sealed trait HunterMsg
  case class AddHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg
  case class MoveHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg
  case class RemoveHunter[P <: Position[P]](hunter: Hunter[P]) extends HunterMsg

  case class WorldEndUpdate[P <: Position[P]](newWorldsEnd: P)

  case class SendBogeys(to: ActorRef)

  case class Flock[P <: Position[P]](boids: Map[Boid[P], P])

  val sightRadius = 60

  val hunterBaseID = 1000000000l
}

import World._

class World[P <: Position[P]](emptyTerritory: Territory[P]) extends Actor {

  import context.dispatcher
  private def newTickingTask(): Cancellable = {
    context.system.scheduler.schedule(tickRate, tickRate, self, InternalTick)
  }

  def stopped: Receive = {
    case Start =>
      context.become(running(newTickingTask(), Map.empty, List.empty)(emptyTerritory))
  }

  def saving(boids: Map[ActorRef, (Boid[P], Long)],
             uis: List[ActorRef],
             identities: Map[ActorRef, String],
             delayedMsgs: List[(ActorRef, Any)])
            (implicit territory: Territory[P]): Receive = {
    case Identity(behaviorClass) =>
      val newIdentities = identities + ((sender, behaviorClass))
      if (newIdentities.keys == boids.keys) {
        val saveInfo = SaveInfo[P](newIdentities.map { case (aRef, s) => (boids(aRef)._1, s) }.toMap)
        uis foreach { _ ! saveInfo }
        context.become(running(newTickingTask(), boids, uis))
        delayedMsgs foreach { case (from, msg) =>
          val execContext = implicitly[ExecutionContext]
          context.system.scheduler.scheduleOnce(0.milli, self, msg)(execContext, from)
        }
      } else {
        context.become(saving(boids, uis, newIdentities, delayedMsgs))
      }

    case a =>
      context.become(saving(boids, uis, identities, (sender, a) :: delayedMsgs))
  }

  override def receive: Receive = stopped

  def running(tickingTask: Cancellable,
              boids: Map[ActorRef, (Boid[P], Long)],
              uis: List[ActorRef])
             (implicit territory: Territory[P]): Receive = {

    case UIFlag =>
      context.become(running(tickingTask, boids, sender :: uis))

    case RequestSaveInfo =>
      context.become(saving(boids, uis, Map.empty, List.empty))
      boids.keys foreach { _ ! Identify }

    case Stop =>
      tickingTask.cancel()
      context.become(stopped)

    case InternalTick =>
      val boidsAndPos = territory.boids
      uis foreach ( _ ! Flock(boidsAndPos) )

    case i: Intention[P] =>
      boids.get(sender) foreach { case (boid, lastMove) =>
        val now = System.currentTimeMillis
        if (now + movementInterval.toMillis >= lastMove) {
          val (newBoids, newTerritory) = applyIntention(sender, i, boid, boids, now)
          context.become(running(tickingTask, newBoids, uis)
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
      context.become(running(tickingTask, boids, uis)
        (territory.add(ah.hunter, ah.hunter.position)))

    case rh: RemoveHunter[P] =>
      context.become(running(tickingTask, boids, uis)
        (territory.remove(rh.hunter)))

    case mh: MoveHunter[P] =>
      val h = mh.hunter
      context.become(running(tickingTask, boids, uis)
        (territory.remove(h).add(h, h.position)))

    case ab: AddBoid[P] =>
      val color = ab.color
      val (boid, pos) = ab.boidInfo.getOrElse((
        Boid(territory.rndVelocity(Boid.defaultSpeed), color),
        territory.rndPosition()
        ))
      val boidActor = ab.boidActor
      context.become(running(tickingTask,
        boids + ((boidActor, (boid, 0l))),
        uis)
        (territory.add(boid, pos)))
      boidActor ! BogeysMsg(boid, Seq.empty)

    case RemoveBoid(boidActor) =>
      val boid_? = boids.get(boidActor)
      boid_? foreach { case (boid, _) =>
        context.become(running(tickingTask,
          boids - boidActor,
          uis)
          (territory.remove(boid)))
      }

    case weu: WorldEndUpdate[P] =>
      context.become(running(tickingTask, boids, uis)
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
                  .filter(b => b.id != boid.id)) // && (b.direction dot boid.velocity.toDirection) > -0.72f))
      .getOrElse(Seq.empty)
  }
}
