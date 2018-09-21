//class 4
def from(n:Int):Stream[Int] = n #:: from(n + 1)
val nats = from(0)
val m4s = nats map ( _ * 4)
m4s.take(4).toList
//sqrt
def sqrtStream(n:Int):Stream[Double] = {
  def improve(guess:Double) = (guess + n / guess)/2
  lazy val guesses : Stream[Double] = 1 #::(guesses map improve)
  guesses
  //原本利用def 一个函数循环计算自己，现在利用lazy val 每次计算后存储
  //和#::函数会让返回的guesses函数不断计算，
  //(guesses map improve) 变为 improve(guesses.head) #:: (guesss.tail.map(impove)----(1)
  //improve(1) #:: guesses.tail.map(improve) 此时计算出guesses = 1 :: 2 #::(guesses.tail.map improve(
  //回到(1)4的右边，guess.tail = 2 #::(guesses.tail.map improve)
  //长得就跟guesses : Stream[Double] = 1 #::(guesses map improve) 一样

}
val sqrt4 = sqrtStream(4).apply(5)//取出Stream列的第6位
val sqrt4_ = sqrtStream(4).take(6).toList//取出序列前6位

case class Employee(name:String, number: Int, index:Int){
  override def toString:String = name + number.toString
}
object Employee{
  def apply(name:String, number:Int):Employee = new Employee(name, number, 0)

}
val e1 = Employee("john", 123)
val e2 = e1.copy(name = "wzk", number = 123)
//val e3 = copy(e2.name, "cba")//这样定义会失败，因为copy是case class 的成员方法


//猜测


