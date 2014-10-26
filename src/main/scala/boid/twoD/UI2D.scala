package boid.twoD

import java.awt.Color
import javax.swing.SwingUtilities

import akka.actor.{ActorRef, Actor}
import boid.{Hunter, MovingEntity, World}

import scala.swing._
import scala.swing.event.{MouseClicked, MouseMoved}

/**
 * Created by markus on 25/10/2014.
 */
object UI2D {
  val width = 1280
  val height = 1024
}

import UI2D._

class UI2D extends Actor { actor =>

  private var boids: Map[MovingEntity[Position2D], Position2D] = Map.empty
  private var hunters: Map[Hunter[Position2D], Position2D] = Map.empty

  private val nextHunterId: () => Long = {
    var currId = World.hunterBaseID
    () => {
      currId += 1
      currId
    }
  }

  private val win = new MainFrame {
    title = "Boids 2D"

    contents = new Panel {
      preferredSize = new Dimension(width, height)

      background = Color.BLACK

      override def paint(g: Graphics2D): Unit = {
        super.paint(g)
        g.setColor(Color.RED)
        boids foreach { case (e, p) =>
          val heading = e.velocity.withSpeed(10).from(p)
          g.drawLine(p.x.toInt, p.y.toInt, heading.x.toInt, heading.y.toInt)
          g.fillOval(heading.x.toInt-2, heading.y.toInt-2, 4, 4)
        }
        g.setColor(Color.ORANGE)
        hunters foreach { case (h, p) =>
          g.fillOval(p.x.toInt-6, p.y.toInt-6, 13, 13)
        }
      }

      listenTo(mouse.moves, mouse.clicks)

      reactions += {
        case MouseMoved(_, pt, _) =>
          actor.self ! World.AddHunter(new Hunter[Position2D](World.hunterBaseID, Direction2D.default, Position2D(pt.x, pt.y)))

        case e@MouseClicked(_, pt, _, _, _) =>
          val button = e.peer.getButton
          val p = Position2D(pt.x, pt.y)
          if (button == 1) {
            val p = Position2D(pt.x, pt.y)
            val h = new Hunter[Position2D](nextHunterId(), Direction2D.default, p)
            actor.self ! World.AddHunter(h)
            hunters = hunters + ((h, p))
          } else if (button == 3) {
            hunters
              .filter { case(h, pH) => pH.distanceTo(p) <= 10 }
              .foreach { case(h, _) =>
                actor.self ! World.RemoveHunter(h)
                hunters = hunters - h
              }
          }
      }
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
      SwingUtilities.invokeLater(new Runnable {
        def run(): Unit = {
          win.repaint()
        }
      })

    case h: World.AddHunter[Position2D] =>
      world ! h
  }
}
