package demo

import akka.actor.{ActorSystem, Props}
import boid.World
import boid.twoD.{UI2D, Territory2D, Position2D}
import com.typesafe.config.ConfigFactory

/**
 * Created by markus on 25/10/2014.
 */
object Runner2D {
  val cfg = ConfigFactory.load
  val system = ActorSystem("boid-system", cfg.getConfig("local"))

  def main(args: Array[String]) {
    val ui = system.actorOf(Props(classOf[UI2D]), s"ui")
    val world = system.actorOf(Props(classOf[World[Position2D]], new Territory2D(UI2D.width, UI2D.height), 100, ui), s"world")
    world ! World.Start
  }
}
