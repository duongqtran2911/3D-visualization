package math
import shape._

/**
  * A class that describes a 3x3 matrix and its related computations.
  */
class Matrix3(_values: Array[Double]) {
  val values :  Array[Double] = _values
    
  def multiply (matrix_ : Matrix3) : Matrix3 = {
      var result : Array[Double] = Array.fill[Double](9)(0)
      for(row <- 0 to 2) {
          for(col <- 0 to 2) {
              for(i <- 0 to 2) {
                  result(row*3 + col) += this.values(row*3+i) * matrix_.values(i*3 + col)
              }
          }
      }
      new Matrix3(result)
  }
    
  def transform (vertex_ : Vertex) : Vertex = new Vertex(
      vertex_.x * this.values(0) + vertex_.y*this.values(3) + vertex_.z * this.values(6),
      vertex_.x * this.values(1) + vertex_.y*this.values(4) + vertex_.z * this.values(7),
      vertex_.x * this.values(2) + vertex_.y*this.values(5) + vertex_.z * this.values(8),
  )
  
}