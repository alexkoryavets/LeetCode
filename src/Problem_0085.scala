import scala.annotation.tailrec

object Problem_0085 extends App {
  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    var top_i = 0
    var bottom_i = 0
    var left_j = 0
    var right_j = 0
    var res = 0
    while ((top_i < matrix.length) && (bottom_i < matrix.length) && (left_j < matrix(0).length) && (right_j < matrix(0).length)) {
      var isStillRectangle = (matrix(top_i)(left_j) == '1')
      var right = true
      do {
        if (right) {
          if (right_j < matrix(0).length)
            right_j += 1
          right = false
        }
        else {
          if (bottom_i < matrix.length)
            bottom_i += 1
          right = true
        }
        isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, isStillRectangle)
      } while (isStillRectangle)
    }
    res
  }

  def isRectangle(top: Int, left: Int, bottom: Int, right: Int, matrix: Array[Array[Char]], isRectangleParam: Boolean): Boolean = {
    if (!isRectangleParam) return isRectangleParam
    if ((top > bottom)||(left > right)) return isRectangleParam
    else {
      //isRectangle &&= for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == "1") true else false}
      var res = (for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == '1') true else false}).fold(isRectangleParam)(_ & _)
      isRectangle(top, left, bottom, right - 1, matrix, res) &&
      isRectangle(top, left, bottom - 1, right, matrix, res)
    }
  }

  val matrix = Array(
    Array('1', '1', '0'),
    Array('1', '1', '0')
  )
  print(isRectangle(0, 0, 1, 0, matrix, true))
  print(isRectangle(0, 0, 1, 1, matrix, true))
  print(isRectangle(0, 0, 1, 2, matrix, true))

}
