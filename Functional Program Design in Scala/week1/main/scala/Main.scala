/**
  * Created by LENOVO on 2016/12/9.
  */
object Main extends App{
  println("hello world")
  val a = List(1, 2, 3) mkString
  val aspace = List(1, 2) mkString "."
  println(a, aspace)
  //partialFunction： worklike functions and can also query whether a function is defined for a given argument

  val f: Int => String = {
    case a:Int => (a + 1).toString
  }
  val f2 : PartialFunction[String, String] ={
    case "ping" => "Pong"
    case a => a
  }
  println(f2.isDefinedAt("ping"))
  println(f2.isDefinedAt("pong"))


}
