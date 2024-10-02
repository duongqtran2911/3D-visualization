import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.geom.Path2D
import java.io.File

import scala.swing._
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position
import scala.swing.Dimension
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event.Key
import scala.swing.event.KeyPressed

import components.Scene
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter

object AppRoom extends SimpleSwingApplication {

  val widthScene = 1500
  val widthMap = 300
  val height = 1000
  val camColorMap: Color = new Color(219, 20, 48)
  val bgMap: Color = new Color(40, 39, 51)
  val colorWallMap: Color = new Color(168, 167, 173)
  val bgColor = new Color(245, 245, 220)
  var graphic: Panel = null
  var chosenFile: JFileChooser = null
  var running: String = ""

  /**
   * Moves the camera inside the GUI.
   */
  def moveCamera(xDirect: Int = 0, yDirect: Int = 0, zDirect: Int = 0) = {
    Scene.proj.moveCam(xDirect, yDirect, zDirect)
    Scene.translate()
    this.graphic.repaint()
  }

  /**
   * Rotates the camera inside the GUI.
   */
  def rotateCamera(angle: Double = 0) = {
    Scene.proj.rotateCam(angle)
    Scene.rotate()
    this.graphic.repaint()
  }

  /**
   * Choose a file from the 'data' package to run the program.
   */
  def chooseFile() = {
    chosenFile = new JFileChooser(new File("."))
    chosenFile.setDialogTitle("Select a File")
    chosenFile.setAcceptAllFileFilterUsed(false)
    chosenFile.addChoosableFileFilter(new FileNameExtensionFilter(".json", "json"))
    val result = chosenFile.showOpenDialog(null)
    if (result == JFileChooser.APPROVE_OPTION) {
      val filePath = chosenFile.getSelectedFile.getAbsolutePath()
      if (filePath.contains("json")) {
        try {
          Scene.init(filePath)
          this.graphic.repaint()
          running = filePath
        } catch {
          case _ :  Throwable => { 
            Dialog.showMessage(top, "Description not written in correct format. End program", "Hey you!")
            sys.exit(0)
          }   
        }
      }
    }
  }

  /**
   * Exits the program.
   */
  def exit() = {
    val res = Dialog.showConfirmation(top, "Do you really want to quit?", title = "Hey you!", optionType = Dialog.Options.YesNo)
    if (res == Dialog.Result.Yes) sys.exit(0)
  }

  /**
   * Resets the program.
   */
  def reset() = {
    Scene.init(running)
    this.graphic.repaint()
  }

  /**
   * Main function of the GUI.
   */
  def top: Frame = {
    running = "src\\data\\SceneCorrectFormat1.json"
    Scene.init(running) // runs default file when program starts.
    graphic = new Panel {
      override def paint(g: Graphics2D) = {
        super.paint(g)
        g.setColor(Color.DARK_GRAY)
        g.fillRect(0, 0, widthScene, height)
        g.translate(widthScene / 2, height / 2)
        g.setColor(Color.BLUE)
        for (wall <- 0 until Scene.room.walls.size) {
          val numSurfaces = if (wall == 0) 5 else 1
          for (wSurface <- 0 to numSurfaces) {
            if (Scene.room.walls(wall).surfaces(wSurface).isFilled) {
              g.setColor(Scene.room.walls(wall).surfaces(wSurface).color)
              g.fill(Scene.room.walls(wall).surfaces(wSurface).shape)
            }
          }
        }
        g.setColor(bgMap)
        g.fillRect(widthScene / 2, -height / 2, widthMap, height)
        g.translate((widthScene + widthMap) / 2, 0)
        val sketch = Scene.room.paths(5)
        g.setColor(colorWallMap)

        for (w <- 0 until Scene.room.walls.size) {
          if (w == 0)
            g.setStroke(new BasicStroke(5))
          else
            g.setStroke(new BasicStroke(1))
          val path = new Path2D.Double()
          for (v <- 0 until sketch.size) {
            val vOnMap = Scene.proj.translateToMap(Scene.room.walls(w).vertices(sketch(v)))
            if (v == 0)
              path.moveTo(vOnMap.x, vOnMap.z)
            else
              path.lineTo(vOnMap.x, vOnMap.z)
          }
          path.closePath()
          if (w != 0)
            g.fill(path)
          else
            g.draw(path)
        }
        val camOnMap = Scene.proj.translateToMap(Scene.proj.getCam())
        g.setColor(camColorMap)
        g.fillOval(camOnMap.x.toInt - 5, camOnMap.z.toInt - 5, 10, 10)
      }
    }
    
    graphic.preferredSize.setSize(new Dimension(widthScene, height))

    val mainPanel = new BorderPanel {
      listenTo(keys)
      reactions += {
        case KeyPressed(_, Key.Up, _, _) =>
          moveCamera(zDirect = -1)
        case KeyPressed(_, Key.Down, _, _) =>
          moveCamera(zDirect = 1)
        case KeyPressed(_, Key.Right, _, _) =>
          moveCamera(xDirect = 1)
        case KeyPressed(_, Key.Left, _, _) =>
          moveCamera(xDirect = -1)
        case KeyPressed(_, Key.D, _, _) =>
          rotateCamera(90)
        case KeyPressed(_, Key.A, _, _) =>
          rotateCamera(-90)
      }
      focusable = true
      requestFocus
    }

    mainPanel.layout(graphic) = Position.Center
    mainPanel.size.setSize(widthScene + widthMap, height)

    val frame = new MainFrame {
      title = "3D Room Visualization"
      resizable = false
      contents = mainPanel
      menuBar = new MenuBar() {
        contents += new Menu("File") {
          contents += new MenuItem(Action("Open") { chooseFile() })
          contents += new MenuItem(Action("Reset") { reset() })
          contents += new Separator()
          contents += new MenuItem(Action("Exit") { exit() })
        }
      }
      size = new Dimension(widthScene + widthMap, height)
    }
    return frame
  }

}