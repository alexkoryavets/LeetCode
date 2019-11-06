object Problem_0893 extends App {
  def numSpecialEquivGroups(A: Array[String]): Int = {
    var res: scala.collection.mutable.Map[String, List[Int]] = scala.collection.mutable.Map[String, List[Int]]()
    var processed: List[Int] = List[Int]()
    var i = 0
    while (i < A.length) {
      if (!(processed.contains(i))) {
        res += (A(i) -> List(i))
        processed :+= i
        var j = i + 1
        while (j < A.length) {
          if (areSpecialEquivGroups(A(i), A(j))) {
            res(A(i)) :+= j
            processed :+= j
          }
          j += 1
        }
      }
      i += 1
    }
    res.keys.size
  }

  def areSpecialEquivGroups(A: String, B: String): Boolean = {
    var odds: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
    var evens: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
    var res = true

    for (i <- 0 to A.length - 1) {
      val chr = A.substring(i, i + 1)
      if (i % 2 == 1)
        if (evens.contains(chr))
          evens(chr) += 1
        else
          evens += (chr -> 1)
      else
        if (odds.contains(chr))
          odds(chr) += 1
        else
          odds += (chr -> 1)
    }

    var i = 0
    while (i < B.length) {
      val chr = B.substring(i, i + 1)
      if (i % 2 == 1)
        if (evens.contains(chr))
          if (evens(chr) == 0) {
            res = false
            i = B.length
          }
          else
            evens(chr) -= 1
        else {
          res = false
          i = B.length
        }
      else
        if (odds.contains(chr)) {
          if (odds(chr) == 0) {
            res = false
            i = B.length
          }
          else
            odds(chr) -= 1
        }
        else {
          res = false
          i = B.length
        }
      i += 1
    }

    res
  }

  print(numSpecialEquivGroups(Array("aabb", "bbaa")))
  print(numSpecialEquivGroups(Array("abcd","cdab","cbad","xyzz","zzxy","zzyx")))
}
