

object class5 {
  //  val in = Source.fromURL("http:lamp.epf.ch/files/content/sites/lamp/files/teaching/progfun/linxuwords")
  val rWords = List("we", "you", "here", "-", "jj", "kk")
  val words = rWords filter (word => word forall (chr => chr.isLetter))
  //we can filter all of non letter characters.
  val mnem1 = Map("2" -> "ABC", "3" -> "DEF", "4" -> "GHI", "5" -> "JKL",
    "6" -> "MNO", "7" -> "PQRS", "8" -> "TUV", "9" -> "WXYZ"
  )
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for {
      (digit, str) <- mnem
      //use mnem1 will triger the problem of type mismatch
      // because "" is string '' is char
      ltr <- str
    } yield ltr -> digit
  //这里不能将for循环用括号括起来，不然会提示这个错误，其实就是找不到返回值

  def wordCode(word: String): String =
//    word map charCode//只有大写字母可以匹配
  word.toUpperCase map charCode
  //here charCode, a map is function itself
  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()
  //groupby is a member function of list

  def encode(number: String): Set[List[String]] ={
    if (number.isEmpty) Set(List())//first we consider boundary case
    else {
      val temp = for {
        split <- 1 to number.length;
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
      temp.toSet
    }
  }
  val e1 = encode("4373")

}
print(class5.words)
print(class5.wordCode("hey"))
print(class5.wordsForNum)
print(class5.e1)



