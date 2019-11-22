object Problem_1043 extends App {
  def maxSumAfterPartitioning(A: Array[Int], K: Int): Int = {
    var res = 0
    scala.util.Sorting.quickSort(A)
    val numGroups = (A.length / K) + {if (A.length % K > 0) 1 else 0}
//    var A_max: Array[Int] = new Array[Int]((A.size / K) + {if (A.size / K - A.size % K > 0) 1 else 0})

    for (i <- 1 to numGroups) {
      res += (A(A.length - i) * {if (i < numGroups) K else if (A.length % K > 0) (A.length % K) else K})
    }
    res
  }

  print(maxSumAfterPartitioning(Array(1, 2, 3, 4, 5), 5))
}
