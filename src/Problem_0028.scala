object Problem_0028 extends App {
  def strStr(haystack: String, needle: String): Int = {
    var res = -1
    var i = 0
    while (i < haystack.size - needle.size + 1) {
      if (haystack.substring(i, i + needle.size).equals(needle)) {
        res = i
        i = haystack.size
      }
      i += 1
    }
    res
  }

  print(strStr("asdfgh", "df") + "\n")
  print(strStr("asdfgh", "") + "\n")
  print(strStr("asdfgh", "dfa") + "\n")
  print(strStr("asdfgh", "asdf") + "\n")
  print(strStr("asdfgh", "h") + "\n")
}
