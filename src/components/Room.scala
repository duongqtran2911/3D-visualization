package components

import scala.collection.mutable.Buffer
import scala.math._
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import shape._
import java.awt.Color

class Room extends Transform {

  var walls = Buffer[Wall]()
  var paths: Array[Array[Int]] = Array.ofDim[Int](6, 4)

  /**
   * Sets up walls and surfaces (paths) with data read from file.
   */
  def setWalls(data: JSONObject) = {
    walls = Buffer[Wall]()
    val jsonPaths = data.get("path").asInstanceOf[JSONArray]
    for (i <- 0 until jsonPaths.size) {
      val jsonPath = jsonPaths.get(i).asInstanceOf[JSONArray]
      for (j <- 0 until jsonPath.size) {
        paths(i)(j) = jsonPath.get(j).asInstanceOf[Long].toInt
      }
    }
    val jsonRoom = data.get("wall").asInstanceOf[JSONArray]
    for (i <- 0 until jsonRoom.size) {
      val jsonWall = jsonRoom.get(i).asInstanceOf[JSONArray]
      val wall = new Wall()
      for (j <- 0 until jsonWall.size) {
        val vtx = jsonWall.get(j).asInstanceOf[JSONObject];
        wall.vertices(j) = new Vertex(
          vtx.get("x").asInstanceOf[Long].toDouble - 100,
          vtx.get("y").asInstanceOf[Long].toDouble - 100,
          vtx.get("z").asInstanceOf[Long].toDouble - 100)
      }
      if (i == 0) wall.setColor(Color.cyan)
      wall.setSurfaces()
      walls += wall
    }
  }

  /**
   * Swap paths/surfaces order of a wall block.
   * Used when drawing and coloring surfaces.
   */
  def swapPaths(a: Int, b: Int) = {
    val t = paths(a)
    paths(a) = paths(b)
    paths(b) = t
  }

  /**
   * Sorts all wall blocks according to distance to the camera.
   */
  def sortWalls() = {
    val roomBox = walls(0)
    var wallToSort = this.walls.drop(1)
    wallToSort = wallToSort.sortBy(_.disToCam).reverse
    this.walls(0) = roomBox
    for (w <- 1 until walls.size) {
      this.walls(w) = wallToSort(w - 1)
    }
  }

  def projectToMap() = {
    val xTranslate = (walls(0).vertices(paths(5)(0)).x + walls(0).vertices(paths(5)(2)).x) / 2
    val zTranslate = (walls(0).vertices(paths(5)(0)).z + walls(0).vertices(paths(5)(2)).z) / 2
    Scene.proj.setMapTranslation(new Vertex(
      xTranslate, 0, zTranslate))
  }

  /**
   * Adjusts the wall blocks and surfaces position to camera's movement.
   */
  override def translate() = {
    for (wall <- walls) {
      for (v <- 0 until wall.vertices.size)
        wall.vertices(v) = Scene.proj.translate(wall.vertices(v))
      wall.setSurfaces()
    }
    sortWalls()
  }

  /**
   * Adjusts the wall blocks and surfaces position to camera's rotation.
   */
  override def rotate() = {
    swapPaths(0, 1)
    swapPaths(2, 3)
    for (wall <- walls) {
      for (v <- 0 until wall.vertices.size)
        wall.vertices(v) = Scene.proj.rotate(wall.vertices(v))
      wall.setSurfaces()
    }
    sortWalls()
  }
}