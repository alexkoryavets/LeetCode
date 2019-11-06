import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

object Problem_0561 extends App {

  def longestPalindromeMinimalCode(s: String): String = {
    val res = ((for { i <- (s.size to 1 by -1).view; s <- s.toSeq.sliding(i).map(_.unwrap) if s == s.reverse} yield s) headOption)
    if (!res.isEmpty) res.get
    else s
  }

  def longestPalindromeWithReverseSub(s: String): String = {
    var maxLen = 0
    var maxLenPos = 0
    for {i <- 0 to s.size - 1} {
      var len = 1;
      var found: Array[Boolean] = Array(false, true)
      do {
        found(len % 2) = isPalindrome(s, i, len)
        len += 1
      } while (found(0)||(found(1)))

      len = len - 3
      if (len > maxLen) {
        maxLen = len
        maxLenPos = i - len/2 + (len + 1)%2
      }
    }
    s.substring(maxLenPos, maxLenPos + maxLen)
  }

  def isPalindrome(s: String, center: Int = 0, length: Int = 1): Boolean = {
    if (length%2 == 1)
      if ((center - length/2 < 0) || (center + length/2 + (length % 2) > s.length)) false
      else
        s.substring((center - length/2), (center + length/2 + (length % 2))) == s.substring((center - length/2), (center + length/2 + (length % 2))).reverse
    else
      if ((center - length/2 + 1 < 0) || (center + length/2 + 1 > s.length)) false
      else
        s.substring((center - length/2 + 1), (center + length/2 + 1)) == s.substring((center - length/2 + 1), (center + length/2 + 1)).reverse
  }

  def longestPalindrome(s: String): String = {
    var maxLen = if (s.length > 0) 1 else 0
    var maxLenPos = 0
    var i = 0
    for (i <- 0 to s.length - 1) {
      var l = 0
      var r = 0
      while ((s.charAt(i - l) == s.charAt(i + r)) && (i + r < s.length - 1))
        r += 1
      if (s.charAt(i - l) != s.charAt(i + r)) r -= 1
      if (i > 0) l += 1
      if (s.charAt(i - l) != s.charAt(i + r)) l -= 1
      while ((s.charAt(i - l) == s.charAt(i + r)) && (i + r < s.length - 1) && (i - l > 0)) {
        l += 1
        r += 1
      }

      if (s.charAt(i - l) != s.charAt(i + r)) {
        l -= 1
        r -= 1
      }

      if (r + l + 1 > maxLen) {
        maxLen = r + l + 1
        maxLenPos = i - l
      }
    }

    s.substring(maxLenPos, maxLenPos + maxLen)
  }

//  var str: String =
  print(longestPalindrome(s = "a") + "\n")
  print(longestPalindrome(s = "bb") + "\n")
  print(longestPalindrome(s = "ccc") + "\n")
  print(longestPalindrome(s = "") + "\n")
  print(longestPalindrome(s = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111") + "\n")
  print(longestPalindrome(s = "jrjnbctoqgzimtoklkxcknwmhiztomaofwwzjnhrijwkgmwwuazcowskjhitejnvtblqyepxispasrgvgzqlvrmvhxusiqqzzibcyhpnruhrgbzsmlsuacwptmzxuewnjzmwxbdzqyvsjzxiecsnkdibudtvthzlizralpaowsbakzconeuwwpsqynaxqmgngzpovauxsqgypinywwtmekzhhlzaeatbzryreuttgwfqmmpeywtvpssznkwhzuqewuqtfuflttjcxrhwexvtxjihunpywerkktbvlsyomkxuwrqqmbmzjbfytdddnkasmdyukawrzrnhdmaefzltddipcrhuchvdcoegamlfifzistnplqabtazunlelslicrkuuhosoyduhootlwsbtxautewkvnvlbtixkmxhngidxecehslqjpcdrtlqswmyghmwlttjecvbueswsixoxmymcepbmuwtzanmvujmalyghzkvtoxynyusbpzpolaplsgrunpfgdbbtvtkahqmmlbxzcfznvhxsiytlsxmmtqiudyjlnbkzvtbqdsknsrknsykqzucevgmmcoanilsyyklpbxqosoquolvytefhvozwtwcrmbnyijbammlzrgalrymyfpysbqpjwzirsfknnyseiujadovngogvptphuyzkrwgjqwdhtvgxnmxuheofplizpxijfytfabx") + "\n")
  print(longestPalindrome("cbbd") + "\n")
  print(longestPalindrome("bbd") + "\n")
//  print(isPalindrome("aba", 1, 3) + "\n")
//  print(isPalindrome("abba", 1, 4) + "\n")
//  print(isPalindrome("abba", 1, 2) + "\n")
//  print(isPalindrome("abba", 1, 3) + "\n")
}
