import scala.annotation.tailrec

object Problem_0085 extends App {
//  //  Too complex code
//  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
//    var top_i = 0
//    var bottom_i = 0
//    var left_j = 0
//    var right_j = 0
//    var res = 0
//    while ((top_i < matrix.length) && (bottom_i < matrix.length - 1) && (left_j < matrix(0).length) && (right_j < matrix(0).length - 1)) {
//      var isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, (matrix(top_i)(left_j) == '1'))
//      var square = 0
//      var canExpandRight = true
//      var canExpandBottom = true
//      var rightMost = 0
//      var bottomMost = 0
//
//      while (isStillRectangle && (canExpandBottom || canExpandRight))
//      {
//        square = (right_j - left_j + 1) * (bottom_i - top_i + 1)
//        if (canExpandRight) {
//          if (right_j < matrix(0).length - 1) {
//            right_j += 1
//          } else {
//            canExpandRight = false
//          }
//        }
//        isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, isStillRectangle)
//
//        if (!isStillRectangle) {
//          right_j -= 1
//          canExpandRight = false
//        }
//
//        if (canExpandBottom) {
//          if (bottom_i < matrix.length - 1) {
//            bottom_i += 1
//          } else {
//            canExpandBottom = false
//          }
//        }
//        isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, true)
//
//        if (!isStillRectangle) {
//          bottom_i -= 1
//          canExpandBottom = false
//        }
//
//      }
//      if (square > res) {
//        res = square
//        rightMost = right_j
//        bottomMost = bottom_i
//      }
//
//      isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, (matrix(top_i)(left_j) == '1'))
//      square = 0
//      canExpandRight = true
//      canExpandBottom = true
//
//      while (isStillRectangle && (canExpandBottom || canExpandRight))
//      {
//        square = (right_j - left_j + 1) * (bottom_i - top_i + 1)
//        if (canExpandBottom) {
//          if (bottom_i < matrix.length - 1) {
//            bottom_i += 1
//          } else {
//            canExpandBottom = false
//          }
//        }
//        isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, isStillRectangle)
//
//        if (!isStillRectangle) {
//          bottom_i -= 1
//          canExpandBottom = false
//        }
//
//        if (canExpandRight) {
//          if (right_j < matrix(0).length - 1) {
//            right_j += 1
//          } else {
//            canExpandRight = false
//          }
//        }
//        isStillRectangle = isRectangle(top_i, left_j, bottom_i, right_j, matrix, true)
//
//        if (!isStillRectangle) {
//          right_j -= 1
//          canExpandRight = false
//        }
//
//      }
//
//      top_i += 1
//      left_j += 1
//    }
//    res
//  }
//
//  def isRectangle(top: Int, left: Int, bottom: Int, right: Int, matrix: Array[Array[Char]], isRectangleParam: Boolean): Boolean = {
//    if (!isRectangleParam) return isRectangleParam
//    if ((top > bottom)||(left > right)) return isRectangleParam
//    else {
//      //isRectangle &&= for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == "1") true else false}
//      var res = (for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == '1') true else false}).fold(isRectangleParam)(_ & _)
//      isRectangle(top, left, bottom, right - 1, matrix, res) &&
//      isRectangle(top, left, bottom - 1, right, matrix, res)
//    }
//  }

  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    var res = if (matrix.length > 0) if (matrix(0).length > 0) (if (matrix(0)(0) == '1') 1 else 0) else 0 else 0
    var top = 0
    var isStillRectangle: Boolean = if (res > 0) true else false
    while (top < matrix.length) {
      var left = 0
      while (left < matrix(0).length) {
        var bottom = top
        var right = left
        isStillRectangle = isRectangle(top, left, bottom, right, matrix)
        while (bottom < (matrix.length - 1) && isStillRectangle) {
          while (right < (matrix(0).length - 1) && isStillRectangle) {
            right += 1
            isStillRectangle = isRectangle(top, left, bottom, right, matrix)
          }
          if (!isStillRectangle) right -= 1
          if (((bottom - top + 1) * (right - left + 1)) > res) {
            res = ((bottom - top + 1) * (right - left + 1))
//            print(s"New one found. Square = $res, top = $top, left = $left, bottom = $bottom, right = $right\n")
          }

          bottom += 1
          right = left
          isStillRectangle = isRectangle(top, left, bottom, right, matrix)
        }
        if (!isStillRectangle) bottom -= 1
        if (((bottom - top + 1) * (right - left + 1)) > res) {
          res = ((bottom - top + 1) * (right - left + 1))
//          print(s"New one found. Square = $res, top = $top, left = $left, bottom = $bottom, right = $right\n")
        }

        bottom = top
        right = left
        isStillRectangle = isRectangle(top, left, bottom, right, matrix)
        while (right < (matrix(0).length - 1) && isStillRectangle) {
          right += 1
          bottom = top
          isStillRectangle = isRectangle(top, left, bottom, right, matrix)
          while (bottom < (matrix.length - 1) && isStillRectangle) {
            bottom += 1
            isStillRectangle = isRectangle(top, left, bottom, right, matrix)
          }
          if (!isStillRectangle) bottom -= 1

          if (((bottom - top + 1) * (right - left + 1)) > res) {
            res = ((bottom - top + 1) * (right - left + 1))
//            print(s"New one found. Square = $res, top = $top, left = $left, bottom = $bottom, right = $right\n")
          }
        }

        left += 1
      }
      top += 1
    }
    res
  }

  def isRectangle(top: Int, left: Int, bottom: Int, right: Int, matrix: Array[Array[Char]]): Boolean = {
    (for {i <- top to bottom; j <- left to right} yield {if (matrix(i)(j) == '1') true else false}).fold(true)(_ & _)
  }
//  val matrix: Array[Array[Char]] = Array(Array('1'))
  print(maximalRectangle(Array(Array('0'))))

  print(maximalRectangle(Array(Array('1'))))

  print(maximalRectangle(Array(Array('0', '1'))))

  print(maximalRectangle(Array(Array('0', '1'), Array('0', '1'))))

  print(maximalRectangle(Array(
    Array('0','0','1','0'),
    Array('1','1','1','1'),
    Array('1','1','1','1'),
    Array('1','1','1','0'),
    Array('1','1','0','0'),
    Array('1','1','1','1'),
    Array('1','1','1','0')
    )
  ))
  
  print(maximalRectangle(
    Array(
      Array('1','1','1','1','1','1','1','1'),
      Array('1','1','1','1','1','1','1','0'),
      Array('1','1','0','1','1','1','1','0'),
      Array('1','1','1','1','1','0','0','0'),
      Array('0','1','1','1','1','0','0','0')
    )
  ))

  print(maximalRectangle(
    Array(
      Array('0', '1', '1'),
      Array('0', '0', '1'),
      Array('1', '1', '1'),
      Array('0', '1', '1'),
      Array('1', '1', '0')
    )
  ))
//  print(isRectangle(0, 0, 1, 0, matrix))
//  print(isRectangle(0, 0, 1, 1, matrix))
//  print(isRectangle(0, 0, 1, 2, matrix))
//  print(isRectangle(0, 0, 2, 2, matrix))
//  print(maximalRectangle(matrix))
}
