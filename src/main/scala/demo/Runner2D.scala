package demo

import akka.actor.{ActorSystem, Props}
import boid.behavior.{StdBehavior, ScaredBehavior}
import boid.{BoidActor, World}
import boid.twoD.{UI2D, Territory2D, Position2D}
import com.typesafe.config.ConfigFactory

/**
 * Created by markus on 25/10/2014.
 */
object Runner2D {
  val cfg = ConfigFactory.load
  val system = ActorSystem("boid-system", cfg.getConfig("local"))

  val stds = 500
  val scareds = 50

  def main(args: Array[String]) {
    val ui = system.actorOf(Props(classOf[UI2D]), s"ui")
    val world = system.actorOf(Props(classOf[World[Position2D]], new Territory2D(UI2D.width, UI2D.height), ui), s"world")
    world ! World.Start
    (0 until stds) foreach { i =>
      world ! World.AddBoid(system.actorOf(Props(classOf[BoidActor[Position2D]], StdBehavior), s"boid_$i"), StdBehavior.color)
    }
    (0 until scareds) foreach { i =>
      world ! World.AddBoid(system.actorOf(Props(classOf[BoidActor[Position2D]], ScaredBehavior), s"boid_${stds+i}"), ScaredBehavior.color)
    }

  }
}
