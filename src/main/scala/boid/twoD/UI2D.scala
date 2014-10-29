package boid.twoD

import java.awt.Color

import akka.actor.{ActorRef, Actor}
import boid.{World, Boid, Hunter}

import scala.swing._
import scala.swing.event._

/**
 * Created by markus on 25/10/2014.
 */
object UI2D {
  val width = 1024
  val height = 768
}

import UI2D._

class UI2D extends Actor { actor =>

  private var boids: Map[Boid[Position2D], Position2D] = Map.empty
  private var hunters: Map[Hunter[Position2D], Position2D] = Map.empty

  private val nextHunterId: () => Long = {
    var currId = World.hunterBaseID
    () => {
      currId += 1
      currId
    }
  }

  private val win = new Frame {
    title = "Boids 2D"

    contents = new Panel {
      preferredSize = new Dimension(width, height)

      background = Color.BLACK

      override def paint(g: Graphics2D): Unit = {
        super.paint(g)
        boids foreach { case (b, p) =>
          g.setColor(new Color(b.color))
          val heading = b.velocity.withSpeed(10).from(p)
          g.drawLine(p.x.toInt, p.y.toInt, heading.x.toInt, heading.y.toInt)
          g.fillOval(heading.x.toInt-2, heading.y.toInt-2, 4, 4)
        }
        g.setColor(Color.ORANGE)
        draggedHunter foreach { case h =>
          g.fillOval(h.position.x.toInt-6, h.position.y.toInt-6, 13, 13)
        }
        hunters foreach { case (h, p) =>
          g.fillOval(p.x.toInt-6, p.y.toInt-6, 13, 13)
        }
      }

      listenTo(mouse.moves, mouse.clicks, this)

      private var draggedHunter: Option[Hunter[Position2D]] = None

      reactions += {
        case MouseMoved(_, pt, _) =>
          actor.self ! World.MoveHunter(new Hunter[Position2D](World.hunterBaseID, Direction2D.default, Position2D(pt.x, pt.y)))

        case e@MouseClicked(_, pt, _, _, _) if e.peer.getButton == 3 =>
          val p = Position2D(pt.x, pt.y)
          hunters
            .filter { case(h, pH) => pH.distanceTo(p) <= 10 }
            .foreach { case(h, _) =>
              actor.self ! World.RemoveHunter(h)
              hunters = hunters - h
            }

        case e@MousePressed(_, pt, _, _, _) if e.peer.getButton == 1 =>
          val p = Position2D(pt.x, pt.y)
          val atPosition = hunters.filter { case(h, pH) => pH.distanceTo(p) <= 10 }
          if (atPosition.isEmpty) {
            val p = Position2D(pt.x, pt.y)
            val h = new Hunter[Position2D](nextHunterId(), Direction2D.default, p)
            draggedHunter = Some(h)
          } else {
            atPosition.foreach { case(h, _) =>
              actor.self ! World.RemoveHunter(h)
              hunters = hunters - h
              draggedHunter = Some(h)
            }
          }

        case e@MouseReleased(_, pt, _, _, _) if e.peer.getButton == 1 =>
          draggedHunter foreach { h =>
            val p = Position2D(pt.x, pt.y)
            val newH = new Hunter[Position2D](h.id, Direction2D.default, p)
            actor.self ! World.AddHunter(newH)
            hunters = hunters + ((newH, p))
          }
          draggedHunter = None

        case MouseDragged(_, pt, _) =>
          draggedHunter match {
            case Some(h) =>
              draggedHunter = Some(new Hunter(h.id, Direction2D.default, Position2D(pt.x, pt.y)))
            case _ =>
              // nothing
          }

        case UIElementResized(src) =>
          actor.self ! World.WorldEndUpdate(Position2D(src.size.width, src.size.height))
      }
    }

    override def closeOperation(): Unit = {
      actor.self ! World.Stop
      System.exit(0)
    }

    centerOnScreen()
  }

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    win.visible = true
  }

  def receive: Receive = {
    case World.Start =>
      context.become(withWorld(sender))
  }

  private def withWorld(world: ActorRef): Receive = {
    case f: World.Flock[Position2D] =>
      boids = f.boids
      win.repaint()

    case h: World.HunterMsg =>
      world ! h

    case wu: World.WorldEndUpdate[Position2D] =>
      world ! wu

    case World.Stop =>
      world ! World.Stop
  }
}
