package boid.twoD

import java.awt.{Dimension, Color}
import java.io.{FileWriter, PrintWriter,  File}

import akka.actor.{Props, ActorRef, Actor}
import argonaut._
import Argonaut._
import boid.behavior.{ScaredBehavior, StdBehavior}
import boid._

import scala.io.Source
import scala.swing._
import scala.swing.event._

/**
 * Created by markus on 25/10/2014.
 */
object UI2D {

  // ************* sim parameters ************** //
  val stds = 300
  val scareds = 30

  private val nextHunterId: () => Long = {
    var currId = World.hunterBaseID
    () => {
      currId += 1
      currId
    }
  }

  // ************* sim control ************** //
  object ResetSim
  object SaveSim
  object LoadSim

  object FileFilter extends javax.swing.filechooser.FileFilter {
    def accept(file: File): Boolean = {
      file.getAbsolutePath.matches(".*\\.flock")
    }

    def getDescription: String = "flock files"
  }

  // ************* UI props ************** //

  val title = "Boids 2D"
  val nameBoxDefault = "Name me!"

  // ************* UI tools ************** //

  val nameColorPattern = "(#[0-9a-fA-F]{6}){0,1}(.*)".r
  def parseName(s: String): (String, Option[Color]) = s match {
    case nameColorPattern(null, name) =>
      (name, None)

    case nameColorPattern(colorString, name) =>
      (name, Some(new Color(Integer.parseInt(colorString.substring(1), 16))))
  }

  val debugCells = false

  // ************* json code for saving / loading sims ************** //

  case class StoredBoid(id: Long, behavior: String, pos: Position2D, velocity: Velocity[Position2D], name: Option[String], color: Int)
  case class StoredHunter(x: Float, y: Float)

  def toStoredBoids(boids: Map[Boid[Position2D], Position2D],
                    nameMap: Map[Boid[Position2D], (String, Option[Color])],
                    behaviorMap: Map[Boid[Position2D], String]): List[StoredBoid] = {
    boids.map { case (b, p) =>
      val behavior = behaviorMap(b)
      nameMap.get(b) match {
        case Some((name, Some(color))) =>
          StoredBoid(b.id, behavior, p, b.velocity, Some(name), color.getRGB)
        case Some((name, None)) =>
          StoredBoid(b.id, behavior, p, b.velocity, Some(name), b.color)
        case None =>
          StoredBoid(b.id, behavior, p, b.velocity, None, b.color)
      }
    }.toList
  }

  def toStoredHunters(hunters: Set[Hunter[Position2D]]): List[StoredHunter] = {
    hunters.map(h => StoredHunter(h.position.x, h.position.y)).toList
  }

  private def xyToHunter: (Float, Float) => StoredHunter = (x ,y) => StoredHunter(x, y)
  private def xyFromHunter: StoredHunter => (Float, Float) = {
    h => (h.x, h.y)
  }

  private def xyToVelocity2D: (Float, Float) => Velocity[Position2D] = Velocity2D.apply
  private def xyFromVelocity2D: Velocity[Position2D] => (Float, Float) = {
    v => (v.components(0), v.components(1))
  }
  implicit def Position2DCodecJson = Argonaut.casecodec2(Position2D.apply, Position2D.unapply)("x", "y")
  implicit def Velocity2DCodecJson = Argonaut.codec2(xyToVelocity2D, xyFromVelocity2D)("x", "y")
  implicit def StoredBoidCodecJson = Argonaut.casecodec6(StoredBoid.apply, StoredBoid.unapply)("id", "behaviorClass", "position", "velocity", "name", "color")
  implicit def StoredHunterCodecJson = Argonaut.casecodec2(StoredHunter.apply, StoredHunter.unapply)("x", "y")
}

import UI2D.{nameBoxDefault, debugCells, parseName, toStoredBoids, toStoredHunters, StoredBoidCodecJson}

class UI2D(world: ActorRef) extends Actor { actor =>

  private var boids: Map[Boid[Position2D], Position2D] = Map.empty
  private var names: Map[Boid[Position2D], (String, Option[Color])] = Map.empty
  private var naming: Option[Boid[Position2D]] = None
  private var hunters: Set[Hunter[Position2D]] = Set.empty

  private var lastMouseX: Int = -1
  private var lastMouseY: Int = -1

  private val win = new Frame {
    title = UI2D.title

    val nameBox = new TextField(nameBoxDefault) { box =>
      opaque = false
      foreground = Color.LIGHT_GRAY
      preferredSize = new Dimension(200, 40)
      minimumSize = preferredSize
      horizontalAlignment = Alignment.Center
      visible = false
      listenTo(box)

      reactions += {
        case FocusGained(_,_,_) =>
          if (naming.map(names.get).flatten == None)
            text = ""

        case EditDone(_) =>
          visible = false
          naming.foreach { b =>
            names += ((b, parseName(text)))
            text = nameBoxDefault
            naming = None
          }
      }
    }

    contents = new FlowPanel {
      preferredSize = new Dimension(World.defaultWidth, World.defaultHeight)
      background = Color.BLACK
      focusable = true
      requestFocus()

      contents += nameBox

      override def paint(g: Graphics2D): Unit = {
        super.paint(g)

        val (width, height) = {
          val s = size
          (s.width, s.height)
        }

        if (debugCells) {
          g.setColor(Color.RED)
          (0 to (width - 1) / Territory2D.cellSpan) foreach { col =>
            val x = col * Territory2D.cellSpan
            val x2 = x + width
            g.drawLine(x, 0, x, height)
            g.drawLine(x2, 0, x2, height)
          }
          (0 to (height - 1) / Territory2D.cellSpan) foreach { row =>
            val y = row * Territory2D.cellSpan
            val y2 = y + height
            g.drawLine(0, y, width, y)
            g.drawLine(0, y2, width, y2)
          }

          if (lastMouseX > -1 && lastMouseY > -1) {
            val (i, j) = (lastMouseX / Territory2D.cellSpan, lastMouseY / Territory2D.cellSpan)

            g.setColor(Color.DARK_GRAY)
            g.fillRect(i * Territory2D.cellSpan + 1,
              j * Territory2D.cellSpan + 1,
              Territory2D.cellSpan - 1,
              Territory2D.cellSpan - 1)
          }
        }

        boids foreach { case (b, p) =>
          val color = new Color(b.color)
          g.setColor(color)
          val head = b.velocity.withSpeed(5).from(p)
          val tail = b.velocity.withSpeed(-5).from(p)
          names.get(b).foreach { case (s, c_?) =>
            g.setColor(Color.WHITE)
            g.drawString(s, head.x.toInt+5, head.y.toInt+5)
            c_?.orElse(Option(color)) foreach { c => g.setColor(c) }
          }
          g.drawLine(tail.x.toInt, tail.y.toInt, head.x.toInt, head.y.toInt)
          g.fillOval(head.x.toInt-2, head.y.toInt-2, 4, 4)
        }
        g.setColor(Color.ORANGE)
        draggedHunter foreach { case h =>
          g.fillOval(h.position.x.toInt-6, h.position.y.toInt-6, 13, 13)
        }
        hunters foreach { case h =>
          g.fillOval(h.position.x.toInt-6, h.position.y.toInt-6, 13, 13)
        }
      }

      listenTo(mouse.moves, mouse.clicks, keys, this)

      private var draggedHunter: Option[Hunter[Position2D]] = None

      reactions += {
        case MouseMoved(_, pt, _) =>
          lastMouseX = pt.x
          lastMouseY = pt.y
          world ! World.MoveHunter(new Hunter[Position2D](World.hunterBaseID, Direction2D.default, Position2D(pt.x, pt.y)))

        case e@MouseClicked(_, pt, _, _, _) if e.peer.getButton == 1 && e.clicks == 2 =>
          val cp = Position2D(pt.x, pt.y)
          boids
            .filter { case (b, p) => cp.distanceTo(p) <= 15 }
            .foreach { case(b, _) =>
              naming = Some(b)
              names.get(b) foreach { case (n, _) =>
                nameBox.text = n
              }
              nameBox.visible = true
              revalidate()
            }

        case e@MouseClicked(_, pt, _, _, _) if e.peer.getButton == 3 && e.clicks == 2 =>
          val p = Position2D(pt.x, pt.y)
          hunters
            .filter { h => h.position.distanceTo(p) <= 10 }
            .foreach { h =>
              world ! World.RemoveHunter(h)
              hunters = hunters - h
            }

        case e@MousePressed(_, pt, _, _, _) if e.peer.getButton == 3 =>
          val p = Position2D(pt.x, pt.y)
          val atPosition = hunters.filter { h => h.position.distanceTo(p) <= 10 }
          if (atPosition.isEmpty) {
            val p = Position2D(pt.x, pt.y)
            val h = new Hunter[Position2D](UI2D.nextHunterId(), Direction2D.default, p)
            draggedHunter = Some(h)
          } else {
            atPosition.foreach { h =>
              world ! World.RemoveHunter(h)
              hunters = hunters - h
              draggedHunter = Some(h)
            }
          }

        case e@MouseReleased(_, pt, _, _, _) if e.peer.getButton == 3 =>
          draggedHunter foreach { h =>
            val p = Position2D(pt.x, pt.y)
            val newH = new Hunter[Position2D](h.id, Direction2D.default, p)
            world ! World.AddHunter(newH)
            hunters = hunters + newH
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
          world ! World.WorldEndUpdate(Position2D(src.size.width, src.size.height))

        case KeyPressed(_, Key.N, Key.Modifier.Control, _) =>
          actor.self ! UI2D.ResetSim

        case KeyPressed(_, Key.S, Key.Modifier.Control, _) =>
          actor.self ! UI2D.SaveSim

        case KeyPressed(_, Key.L, Key.Modifier.Control, _) =>
          actor.self ! UI2D.LoadSim
      }
    }

    override def closeOperation(): Unit = {
      world ! World.Stop
      context.system.stop(actor.self)
      context.system.shutdown()
      System.exit(0)
    }

    centerOnScreen()
  }

  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    win.visible = true
    // start the sim
    world ! World.Start

    // register as an UI
    world ! World.UIFlag

    createBoids()
  }

  private var boidActors: List[ActorRef] = List.empty

  private def createBoids(): Unit = {
    // add some boids to the sim
    (0 until UI2D.stds) foreach { _ =>
      val actorRef: ActorRef = context.actorOf(Props(classOf[BoidActor[Position2D]], new StdBehavior))
      boidActors = actorRef :: boidActors
      world ! World.AddBoid(actorRef, StdBehavior.color)
    }
    (0 until UI2D.scareds) foreach { _ =>
      val actorRef: ActorRef = context.actorOf(Props(classOf[BoidActor[Position2D]], new ScaredBehavior))
      boidActors = actorRef :: boidActors
      world ! World.AddBoid(actorRef, ScaredBehavior.color)
    }
  }

  private def clearSim(): Unit = {
    naming = None

    boidActors foreach { b =>
      world ! World.RemoveBoid(b)
      context.stop(b)
    }
    boidActors = List.empty
    names = Map.empty
    boids = Map.empty

    hunters foreach { h =>
      world ! World.RemoveHunter(h)
    }
    hunters = Set.empty
  }

  def receive: Receive = standard

  import UI2D.{StoredBoid, StoredHunter}

  def standard: Receive = {
    case f: World.Flock[Position2D] =>
      boids = f.boids
      win.repaint()

    case UI2D.ResetSim =>
      clearSim()
      context.become(waitingClear(createBoids()))

    case UI2D.SaveSim =>
      world ! World.RequestSaveInfo
      context.become(waitingData)

    case UI2D.LoadSim =>
      val c = new FileChooser(new File(System.getProperty("user.home"))) {
        fileFilter = UI2D.FileFilter
      }

      if (c.showOpenDialog(win.contents.head) == FileChooser.Result.Approve) {
        val json = Source.fromFile(c.selectedFile).getLines().toSeq
        try {
          val storedBoids = json(0).decodeOption[List[StoredBoid]].getOrElse(List.empty)
          val storedHunters = json(1).decodeOption[List[StoredHunter]].getOrElse(List.empty)
          clearSim()
          context.become(waitingClear(fromStorage(storedBoids, storedHunters)))
        } catch {
          case e: Exception =>
            // ignore
            println("load failure!")
        }
      }
  }

  private def waitingData: Receive = {
    case si: World.SaveInfo[Position2D] =>
      val c = new FileChooser(new File(System.getProperty("user.home"))) {
        fileFilter = UI2D.FileFilter
      }

      if (c.showSaveDialog(win.contents.head) == FileChooser.Result.Approve) {
        val file = c.selectedFile
        val writer = new PrintWriter(new FileWriter(file))
        writer.write(toStoredBoids(boids, names, si.data).asJson.nospaces)
        writer.write("\r\n")
        writer.write(toStoredHunters(hunters).asJson.nospaces)
        writer.close()
      }

      context.become(standard)
  }

  private def waitingClear(action: => Unit): Receive = {
    case f: World.Flock[Position2D] if f.boids.isEmpty =>
      action
      context.become(standard)
  }

  private def fromStorage(storedBoids: List[StoredBoid], storedHunters: List[StoredHunter]): Unit = {
    storedBoids foreach { case StoredBoid(id, behaviorClass, p, v, name_?, color) =>
      val boid = new Boid(v, color, id)
      name_? foreach ( name => names = names + ((boid, (name, None))) )
      val actorRef: ActorRef = context.actorOf(Props(classOf[BoidActor[Position2D]], Class.forName(behaviorClass).newInstance()))
      boidActors = actorRef :: boidActors
      world ! World.AddBoid(actorRef, boid.color, Some((boid, p)))
    }

    storedHunters foreach { h =>
      val hunter = new Hunter(UI2D.nextHunterId(), new Velocity2D(0, 0), Position2D(h.x, h.y))
      world ! World.AddHunter(hunter)
      hunters = hunters + hunter
    }
  }
}
