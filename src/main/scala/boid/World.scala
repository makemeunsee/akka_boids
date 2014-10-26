package boid

import akka.actor._
import boid.behavior.{StdBehavior, ScaredBehavior}
import scala.concurrent.duration._

/**
 * Created by markus on 25/10/2014.
 */
object World {
  val tickRate = 20.millis
  val movementInterval = 20.millis

  private object InternalTick
  object Start
  object Stop

  case class AddBoid(boid: ActorRef)
  case class RemoveBoid(boid: ActorRef)
  case class AddHunter[P <: Position[P]](hunter: Hunter[P])
  case class RemoveHunter[P <: Position[P]](hunter: Hunter[P])

  case class SendBogeys(to: ActorRef)

  case class Flock[P <: Position[P]](boids: Map[MovingEntity[P], P])

  val sightRadius = 20

  val hunterBaseID = 1000000000l
}

import World._

class World[P <: Position[P]](emptyTerritory: Territory[P],
                              boidNumber: Int,
                              ui: ActorRef) extends Actor {

  import context.dispatcher
  private def newTickingTask(): Cancellable = {
    context.system.scheduler.schedule(tickRate, tickRate, self, InternalTick)
  }

  def stopped: Receive = {
    case Start =>
      context.become(running(newTickingTask(), Map.empty)(emptyTerritory))
      (0 until boidNumber) foreach { i =>
        self ! AddBoid(context.actorOf(Props(classOf[BoidActor[P]], new StdBehavior), s"boid_$i"))
      }
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
      ui ! Flock(territory.entities.filter{ case (e, _) => e.allegiance == Boid.boidFaction })

    case i: Intention[P] =>
      val (newBoids, newTerritory) = applyIntention(sender, i, boids)
      context.become(running(tickingTask, newBoids)
                    (newTerritory))

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

    case AddBoid(boidActor) =>
      val newBoid = Boid(territory.rndVelocity(Boid.defaultSpeed))
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
  }

  private def applyIntention(sender: ActorRef, i: Intention[P], boids: Map[ActorRef, (Boid[P], Long)])
                            (implicit territory: Territory[P]): (Map[ActorRef, (Boid[P], Long)], Territory[P]) = {
    boids
      .get(sender)
      .map { case (boid, lastMove) =>
        val (newTerritory, newBoid_?) = applyIntention(boid, i)
        val newBoids = newBoid_?
          .map { b =>
          boids + ((sender, (b, System.currentTimeMillis)))
        }
        .getOrElse(boids)
        val diff = System.currentTimeMillis - lastMove
        if (diff >= movementInterval.toMillis) {
          sender ! BogeysMsg(boid, around(boid)(newTerritory))
        } else {
          context.system.scheduler.scheduleOnce(movementInterval - diff.millis,
            self,
            SendBogeys(sender))
        }
        (newBoids, newTerritory)
      }
      .getOrElse((boids, territory))
  }

  private def applyIntention(boid: MovingEntity[P],
                             i: Intention[P])
                            (implicit territory: Territory[P]): (Territory[P], Option[Boid[P]]) = {
    territory.positionOf(boid) match {
      case None =>
        (territory, None)

      case Some(oldPos) =>
        val newBoid = new Boid(i.velocity, boid.id)
        (territory.move(newBoid, i.velocity.from(oldPos)), Some(newBoid))
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
