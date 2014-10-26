package boid.twoD

import java.awt.Color
import javax.swing.SwingUtilities

import akka.actor.{ActorRef, Actor}
import boid.{Hunter, MovingEntity, World}

import scala.swing._
import scala.swing.event.MouseMoved

/**
 * Created by markus on 25/10/2014.
 */
object UI2D {
  val width = 800
  val height = 600
}

import UI2D._

class UI2D extends Actor { actor =>

  private var boids: Map[MovingEntity[Position2D], Position2D] = Map.empty

  private val win = new MainFrame {
    title = "Boids 2D"

    contents = new Panel {
      preferredSize = new Dimension(width, height)

      background = Color.BLACK

      override def paint(g: Graphics2D): Unit = {
        super.paint(g)
        g.setColor(Color.RED)
        boids foreach { case (e, p) =>
          val heading = e.movingVector.fromPosition(p, 10)
          g.drawLine(p.x.toInt, p.y.toInt, heading.x.toInt, heading.y.toInt)
        }
      }

      listenTo(mouse.moves)

      reactions += {
        case MouseMoved(_, pt, _) =>
          actor.self ! World.AddHunter(new Hunter[Position2D](World.hunterId, Direction2D.default, Position2D(pt.x, pt.y)))

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
