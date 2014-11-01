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

  def main(args: Array[String]) {
    val world = system.actorOf(Props(classOf[World[Position2D]], new Territory2D(World.defaultWidth, World.defaultHeight)), s"world")
    system.actorOf(Props(classOf[UI2D], world), s"ui")
  }
}
