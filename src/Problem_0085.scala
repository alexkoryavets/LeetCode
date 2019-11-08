object Problem_0085 extends App {
  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    var i = 0
    var min_i = 0
    var j = 0
    var min_j = 0
    var res = 0
    while (i < matrix.length) {

    }
    res
  }

  def isRectangle(top: Int, left: Int, bottom: Int, right: Int, matrix: Array[Array[Char]], isRectangle: Boolean): Boolean = {
    if (!isRectangle) isRectangle
    if ((top > bottom)||(left > right)) isRectangle
    else {
      //isRectangle &&= for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == "1") true else false}
      var res =   for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == '1') true else false}
      isRectangle && res.fold(isRectangle)(_ & _)
    }
  }

  val matrix = Array(
    Array('1', '1', '0'),
    Array('1', '1', '0')
  )
  print(isRectangle(0, 0, 1, 0, matrix, false))

}
