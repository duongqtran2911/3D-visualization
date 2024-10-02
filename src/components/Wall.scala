package components

import shape.Vertex
import java.awt.Polygon
import java.awt.Polygon
import java.awt.Color

class Surface(_shape: Polygon, _color: Color, _filled: Boolean = true, _distance: Double = 0) {
  var shape: Polygon = _shape
  var color: Color = _color
  var isFilled: Boolean = _filled       // a variable to determine if the surface is visible & hence drawn/filled or not.
  var distance2Cam: Double = _distance
}

class Wall() {

  val vertices = new Array[Vertex](8)
  val surfaces = new Array[Surface](6)
  private var color = Color.GRAY
  var disToCam: Double = 0

  /**
   * Determines the color shades of a surface based on distance to the camera.
   */
  def getColor(distance: Double): Color = {
    val dis = Scene.proj.normalize(distance)
    val redLinear = Math.pow(color.getRed(), 2.4) * dis
    val greenLinear = Math.pow(color.getGreen(), 2.4) * dis
    val blueLinear = Math.pow(color.getBlue(), 2.4) * dis
    val red = Math.pow(redLinear, 1 / 2.4).toInt
    val green = Math.pow(greenLinear, 1 / 2.4).toInt
    val blue = Math.pow(blueLinear, 1 / 2.4).toInt
    return new Color(red, green, blue)
  }

  def setColor(_color: Color) = {
    this.color = _color
  }

  /**
   * Calculates the distance from the center of a wall block to the camera.
   */
  def setDisToCam() = {
    val a = (vertices(0).x + vertices(6).x) / 2 - Scene.proj.getCam().x
    val b = (vertices(0).y + vertices(6).y) / 2 - Scene.proj.getCam().y
    val c = (vertices(0).z + vertices(6).z) / 2 - Scene.proj.getCam().z
    this.disToCam = Math.sqrt(a * a + b * b + c * c)
  }

  /**
   * Swap surfaces order of a wall block
   * Used when drawing and coloring surfaces.
   */
  private def swapSurface(a: Int, b: Int) = {
    val t = this.surfaces(a)
    this.surfaces(a) = this.surfaces(b)
    this.surfaces(b) = t
  }

  /**
   * Makes a surface based on whether it is visible & distance to camera.
   */
  def makeSurface(shape: Array[Int]): Surface = {
    val cam = Scene.proj.getCam()
    val xPoints = new Array[Int](shape.size)
    val yPoints = new Array[Int](shape.size)
    val verticesForShape = new Array[Vertex](4)
    var count = 0
    for (v <- 0 until shape.size) {
      val pointIn2D = Scene.proj.project(vertices(shape(v)))
      xPoints(v) = pointIn2D.x.toInt
      yPoints(v) = pointIn2D.y.toInt
      verticesForShape(v) = vertices(shape(v))
      if (vertices(shape(v)).z > cam.z)
        count += 1
    }
    val isFilled = if (count == shape.size) false else true
    val polygon = new Polygon(xPoints, yPoints, shape.size)
    val distance = Scene.proj.getDistancesCam2Plane(verticesForShape)
    val colorSurface = getColor(distance)
    new Surface(polygon, colorSurface, isFilled, distance)
  }

  /**
   * Sets up all surfaces.
   * Swaps the drawing order if necessary based on distance to camera.
   */
  def setSurfaces() = {
    for (i <- 0 until Scene.room.paths.size) {
      surfaces(i) = makeSurface(Scene.room.paths(i))
    }
    if (surfaces(0).distance2Cam > surfaces(2).distance2Cam) {
      swapSurface(0, 2)
    }
    if (surfaces(1).distance2Cam > surfaces(3).distance2Cam) {
      swapSurface(1, 3)
    }
    if ((!surfaces(1).isFilled || !surfaces(3).isFilled) && color != Color.cyan) {
      surfaces(3).isFilled = false
      surfaces(1).isFilled = false
    }
    setDisToCam()
  }

  /**
   * Gives a description of a vertex.
   */
  override def toString(): String = {
    var vtxStr: String = ""
    for (v <- vertices) {
      vtxStr += v.x + " - " + v.y + " - " + v.z + "\n"
    }
    vtxStr
  }
}