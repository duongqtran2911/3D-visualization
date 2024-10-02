package components

import java.io.FileReader
import org.json.simple.parser.JSONParser
import org.json.simple.JSONObject
import shape.Vertex

object Scene {
  var room: Room = new Room()
  var proj: Projection = new Projection()

  /**
   * Reads the json file and returns data as JSONObject.
   * The json file must be in this format:
   * {
   *   "room": {
   *     "wall": [
   *       [{ vertex coords. }, 
   *        { vertex coords. }, 
   *        { vertex coords. }, 
   *        { vertex coords. }, 
   *        { vertex coords. }, 
   *        { vertex coords. }, 
   *        { vertex coords. }
   *       ] 
   *     ], 
   *     "path": [
   *       [index, index, index, index]
   *     ]
   *   },
   *   "eye": {
   *     vertex coords. 
   *   }
   * }
   * 
   */
  def read(filePath: String) = {
    val jsonParser = new JSONParser()
    val reader = new FileReader(filePath)
    val obj = jsonParser.parse(reader).asInstanceOf[JSONObject]
    val eyeObj = obj.get("eye").asInstanceOf[JSONObject]
    room = new Room()
    proj = new Projection()
    proj.setCam(
      new Vertex(
        eyeObj.get("x").asInstanceOf[Long].toDouble,
        eyeObj.get("y").asInstanceOf[Long].toDouble,
        eyeObj.get("z").asInstanceOf[Long].toDouble))
    val dataRoom = obj.get("room").asInstanceOf[JSONObject]
    room.setWalls(dataRoom)

  }

  /**
   * Adjusts all elements in room to camera's movement.
   */
  def translate() = {
    room.translate()
    room.projectToMap()
    proj.setTranslation(new Vertex(0, 0, 0))
  }

  /**
   * Adjusts all elements in room to camera's rotation.
   */
  def rotate() = {
    room.rotate()
    room.projectToMap()
    proj.setRotation(0)
  }

  /**
   * Initiate the program.
   */
  def init(filePath: String) = {
    read(filePath)
    translate()
  }

}