/**
  * Created by LENOVO on 2016/12/10.
  */
import java.util.Random
object RandomGenerator extends App {
  //随机数产生器
  val rand = new Random//产生器
  val randInt:Int = rand.nextInt
  println(rand)
  println(randInt)

  trait Generator[+T]{
    self => //这是取self为“this"的别名，这个this指的Generator trait的this
    def generate: T

    def map[S](f:T => S):Generator[S] = new Generator[S]{
      def generate = f(self.generate)
    }
    def flatMap[S](f : T => Generator[S]) : Generator[S] =
      new Generator[S]{
        def generate = f(self.generate).generate
      }
  }
  val integers = new Generator[Int]{
    val rand = new Random
    def generate = rand.nextInt()
  }
  println(integers)//intergers仍是一个产生器
  println(integers.generate)//调用其generate函数方可得到一个对应的随机值

//  //第一种定义方式
//  val booleans = new Generator[Boolean]{
//    def generate = integers.generate > 0
//  }
//  val pairs = new Generator[(Int, Int)]{
//    def generate = (integers.generate, integers.generate)
//  }
  //第二种定义方式,我们希望更简洁的表达,如下面
  val booleans = for ( x <- integers) yield (x > 0)
  //T和U是类型名

  def pairs[T,U](t: Generator[T], u: Generator[U]) = for{
    x <- t
    y <- u
  } yield (x ,y)
  val int1 = pairs(integers, integers)
  println("int1:" + int1.generate)
  println(booleans.generate)
  //利用for的转换,第二种定义方式可以改下为下面的形式
  //val booleans = integers map (x => x >0)
  //def pairs[T, U](t:Generator[T], u: Generator[U]) =
  // t flatMap( x => u map ( y => (x, y)))
  //因此需要的trait Genertor补充 map和flatmap函数

//  def map[S](f: T => S): Generator[S] = new Generator[S] {
//    def generate = f(self.generate)
//  } map函数结构跟普通的List的map函数不同，返回的是一个创造器
//  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
//    def generate = f(self.generate).generate
//  }

  //for的这种定义方式比较奇怪,但其目的是利用for语句的见解甩开冗长的new generator定义!!!
  //for语句在scala编译器被转换为map flatmap函数，因此只需自定义map和flatmap即可

  def single[T](x : T): Generator[T] = new Generator[T]{
    def generate = x
  }
  def choose(lb: Int, hb: Int):Generator[Int] =
    for (x <- integers ) yield lb + x % (hb - lb)

  def oneOf[T](xs : T*):Generator [T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)
  def pairs2[T, U](t : Generator[T], u: Generator[U]) :Generator[(T,U)] =
    for ( x <- t; y <- u) yield (x, y)

  println(pairs2(booleans, integers).generate)
  //oneOf函数不返回值，而是通过调用oneOf.generate返回，一旦启用generate就跟
  //for语句表示的一样，idx就是一个choose返回的int，然后xs（idx)返回Array xs的元素

  //产生随机整数list
  def lists: Generator[List[Int]] = for{
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
  def emptyLists = single(Nil)
  def nonEmptyLists = for{
    head <- integers
    tail <- lists
  } yield head :: tail

  //！！！随机测试
  def test[T](g: Generator[T], numTimes: Int = 100)
             (testFunction: T => Boolean): Unit = {//Unit代表没有返回值
    for (i <- 0 until numTimes) {//限制循环次数，注意这个for是没有yield的
      val value = g.generate
      assert(testFunction(value), "test failed for "+value)
    }
    println("passed "+numTimes+" tests")
  }
  println("test result:\n")
  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ++ ys).length > xs.length
  }





}
