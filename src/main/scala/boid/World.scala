package boid

import akka.actor._
import scala.concurrent.duration._

/**
 * Created by markus on 25/10/2014.
 */
object World {
  val tickRate = 30.millis
  private object InternalTick
  object Start
  object Stop

  case class AddBoid(boid: ActorRef)
  case class RemoveBoid(boid: ActorRef)
  case class AddHunter[P <: Position[P]](hunter: Hunter[P])

  case class Flock[P <: Position[P]](boids: Map[MovingEntity[P], P])

  val sightRadius = 20

  val hunterId = 999l
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
              boids: Map[ActorRef, Boid[P]])
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
      boids foreach { case (actor, boid) =>
        actor ! BogeysMsg(boid, around(boids(actor)))
      }

    case i: Intention[P] =>
      boids.get(sender) foreach { entity =>
        val (newTerritory, newBoid_?) = applyIntention(entity, i)
        val newBoids = newBoid_?.map(b => boids + ((sender, b))).getOrElse(boids)
        context.become(running(tickingTask, newBoids)
                              (newTerritory))
      }

    case ah: AddHunter[P] =>
      context.become(running(tickingTask, boids)
        (territory.add(ah.hunter, ah.hunter.position)))

    case AddBoid(boid) =>
      val newEntity = Boid(territory.rndDirection())
      context.become(running(tickingTask,
        boids + ((boid, newEntity)))
        (territory.add(newEntity, territory.rndPosition())))

    case RemoveBoid(boid) =>
      val entity = boids.get(boid)
      entity foreach { e =>
        context.become(running(tickingTask,
          boids - boid)
          (territory.remove(e)))
      }
  }

  private def applyIntention(boid: MovingEntity[P],
                             i: Intention[P])
                            (implicit territory: Territory[P]): (Territory[P], Option[Boid[P]]) = {
    territory.positionOf(boid) match {
      case None =>
        (territory, None)

      case Some(oldPos) =>
        val newBoid = new Boid(i.direction, boid.id)
        (territory.move(newBoid, i.direction.fromPosition(oldPos, i.speed)), Some(newBoid))
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
