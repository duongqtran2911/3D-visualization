package components

import java.awt.Polygon
import scala.math._
import math.Matrix3
import shape.Vertex

class Projection {

  private var cam: Vertex = null
  private var translation: Vertex = new Vertex(0, 0, 0)
  private var mapTranslation: Vertex = new Vertex(0, 0, 0)
  private var mapScale: Double = 0.1
  private var angle: Double = 0.0
  val delta: Double = 10
  private val size: Double = 50

  
  def setCam(_cam: Vertex) = {
    cam = _cam
  }

  def getCam(): Vertex = cam

  /**
   * Projects a 3D vertex onto the screen through a 'vanish point'.
   * Returns its new coordinates.
   */
  def project(vtx: Vertex): Vertex = {
    val r = if (vtx.z > cam.z) (cam.z - 1) else vtx.z
    new Vertex(
      vtx.x / ((-1 / cam.z) * r + 1),
      vtx.y / ((-1 / cam.z) * r + 1),
      0)
  }

  def setMapTranslation(_tran: Vertex) = {
    mapTranslation = _tran
  }

  def translateToMap(_v: Vertex) = {
    new Vertex(
      (_v.x - mapTranslation.x) * mapScale,
      0,
      (_v.z - mapTranslation.z) * mapScale)
  }

  def setTranslation(_tran: Vertex) = {
    translation = _tran
  }

  /**
   * Adjusts a vertex to the camera's movement.
   * Returns its new coordinates.
   */
  def translate(vtx: Vertex): Vertex = {
    new Vertex(
      vtx.x - translation.x,
      vtx.y - translation.y,
      vtx.z - translation.z)
  }

  def setRotation(_angle: Double) = {
    angle = _angle
  }

  /**
   * Adjusts a vertex to the camera's rotation.
   * Returns its new coordinates.
   */
  def rotate(vtx: Vertex): Vertex = {
    val toCam = new Vertex(
      vtx.x - cam.x,
      vtx.y - cam.y,
      vtx.z - cam.z
    )
    val yMatrix = new Matrix3(Array[Double](
      cos(angle), 0, -sin(angle),
      0, 1, 0,
      sin(angle), 0, cos(angle)))
    val rotatedPoint = yMatrix.transform(toCam)
    return new Vertex(
      rotatedPoint.x + cam.x,
      rotatedPoint.y + cam.y,
      rotatedPoint.z + cam.z,
    )
  }

  /**
   * Moves the camera in desired directions.
   * Parameters only take values -1, 0, 1.
   */
  def moveCam(xDirect: Int, yDirect: Int, zDirect: Int) = {
    require(abs(xDirect) < 2, "xDirect signals direction permission, not actualy value")
    require(abs(yDirect) < 2, "yDirect signals direction permission, not actualy value")
    require(abs(zDirect) < 2, "zDirect signals direction permission, not actualy value")
    setTranslation(new Vertex(
      translation.x + xDirect * delta,
      translation.y + yDirect * delta,
      translation.z + zDirect * delta))
    if (!isMoved) setTranslation(new Vertex(0, 0, 0))
  }

  def rotateCam(angle: Double) = {
    setRotation(toRadians(angle))
  }

  /**
   * Returns the components of a plane's equation from given points.
   * (P): ax + by + cz + d = 0.
   */
  def getPlane(vertices: Array[Vertex]): (Double, Double, Double, Double) = {
    val a1 = vertices(1).x - vertices(0).x
    val b1 = vertices(1).y - vertices(0).y
    val c1 = vertices(1).z - vertices(0).z
    val a2 = vertices(2).x - vertices(0).x
    val b2 = vertices(2).y - vertices(0).y
    val c2 = vertices(2).z - vertices(0).z
    val a = b1 * c2 - b2 * c1
    val b = a2 * c1 - a1 * c2
    val c = a1 * b2 - b1 * a2
    val d = (-a * vertices(0).x - b * vertices(0).y - c * vertices(0).z)
    return (a, b, c, d)
  }

  /**
   * Calculates the distance from the camera to a plane.
   */
  def getDistancesCam2Plane(vertices: Array[Vertex]): Double = {
    val (a, b, c, d) = getPlane(vertices)
    abs((a * cam.x + b * cam.y + c * cam.z + d)) / (sqrt(a * a + b * b + c * c))
  }

  /**
   * Determines if camera can be moved or not.
   */
  def isMoved(): Boolean = {
    var isMoved = true
    for (wall <- Scene.room.walls) {
      for (p <- 0 until 4) {
        val verticesPlane = new Array[Vertex](4)
        verticesPlane(0) = translate(wall.vertices(Scene.room.paths(p)(0)))
        verticesPlane(1) = translate(wall.vertices(Scene.room.paths(p)(1)))
        verticesPlane(2) = translate(wall.vertices(Scene.room.paths(p)(2)))
        verticesPlane(3) = translate(wall.vertices(Scene.room.paths(p)(3)))

        val (a, b, c, d) = getPlane(verticesPlane)
        val interPointNS = new Vertex(0, 0, -d.toDouble / c)
        val interPointWE = new Vertex((-d - c * cam.z).toDouble / a, 0, cam.z)
        val xPointsNS = new Array[Int](4)
        val yPointsNS = new Array[Int](4)
        val xPointsWE = new Array[Int](4)
        val yPointsWE = new Array[Int](4)
        for (v <- 0 until verticesPlane.size) {
          xPointsNS(v) = verticesPlane(v).x.toInt
          yPointsNS(v) = verticesPlane(v).y.toInt
          xPointsWE(v) = verticesPlane(v).z.toInt
          yPointsWE(v) = verticesPlane(v).y.toInt
        }
        val surfaceNS = new Polygon(xPointsNS, yPointsNS, verticesPlane.size)
        val surfaceWE = new Polygon(xPointsWE, yPointsWE, verticesPlane.size)
        val containsPointNS = surfaceNS.contains(interPointNS.x, interPointNS.y)
        val containsPointWE = surfaceWE.contains(interPointWE.z, interPointWE.y)

        if (containsPointNS || containsPointWE) {
          if (getDistancesCam2Plane(verticesPlane) < size) {
            isMoved = false
          }
        }
      }
    }
    isMoved
  }

  /**
   * Rescale the coordinates to a range suitable for colors.
   */
  def normalize(_distance: Double): Double = {
    val minVal = 0
    val maxVal = 1000
    val minRange = 0
    val maxRange = 1
    var normalized = (_distance - minVal) / (maxVal - minVal)
    1 - normalized
  }

}