package shape

class Vertex(val _x : Double, val _y : Double, val _z : Double) {
  var x: Double = _x
  var y: Double = _y
  var z: Double = _z 
  
  override def toString() = x.toString() + " - " + y.toString() + " - " + z.toString()
}