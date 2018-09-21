package revise1

/**
  * Created by LENOVO on 2017/3/3.
  *this is my implementation of class Stream. here I avoid using case class of cons. Instead, I just defined a method called "cons"
  */
object MyStreamObject extends App{

  abstract class MyStream[+A] {


    def head: A

    def isEmpty: Boolean

    def tail: MyStream[A]

    def filter(p: A => Boolean): MyStream[A] = {
      if (isEmpty) this
      else if (p(head)) cons(head, tail.filter(p))
      else tail.filter(p)
    }

    def take(n: Int): MyStream[A] = {
      if (n == 0) `empty`//`empty` means  MyStreamObject.empty
      else cons(head, tail.take(n - 1))
    }

    def apply(n: Int): A = if (n == 0) head else tail.apply(n - 1)



    def toList(): List[A] = this match {
      case `empty` => Nil
      case _ => {head :: tail.toList
      }
    }

    def #++[S >: A] (that: MyStream[S]):MyStream[S]={//S >: A  means we can accept elements of superclass of A
    //covariant type occurs in contravariant position
    def getNewStream(lS:MyStream[S], rS: MyStream[S]):MyStream[S]= lS.tail match {
      case  `empty` => cons(lS.head,rS)//
      case _ => cons(lS.head, getNewStream(lS.tail, rS))
    }
      getNewStream(this, that)
    }
    def #::(i: Int) = {//!!! remember operator end up with colon is associative to right
      cons(i, this)
    }
    def map[T](f : A => T):MyStream[T]= this match{
      case `empty` => `empty`
      case _ => cons(f(head), tail.map(f))
    }
    def flatMap[T](f: A => MyStream[T]): MyStream[T] = this match{
      case `empty` => `empty`
      case _ => f(head) #++ tail.flatMap(f)
    }
    def foldLeft[T](zero: T)(f : (T, A) => T): T = this match{// if from(0).foldLeft(f), this will loop forever thuse casues stack overflow
      case `empty` => zero
      case  _ => tail.foldLeft(f(zero, head))(f)
    }
    def takeWhile(f: A => Boolean): MyStream[A] = this match {//
      case `empty` => `empty`
      case _ => if (f(head)) cons(head, tail.takeWhile(f)) else `empty`
    }
    def zip[U](that: MyStream[U]):MyStream[(A, U)] = (this, that) match {
      case (`empty`, _) => `empty`// last elements must be empty
      case (_, `empty`) => `empty`
      case (_, _) => cons((this.head, that.head), (tail.zip(that.tail)))
      }
    def length():Int = {
      val bound = 1000
      def getSize(self: MyStream[A], acc:Int):Int = self match{
        case `empty` => acc
        case _ => if (acc > bound - 2) -1 else getSize(self.tail, acc + 1)
      }
      getSize(this, 0)

    }



  }



  def from(n: Int): MyStream[Int] = {
    cons(n, from(n + 1)) // all the positive integers start from n
  }
  case object empty extends MyStream[Nothing]{
    def head = throw new NoSuchElementException("empty head")//

    def tail = throw new NoSuchElementException("empty tail")

    override def isEmpty: Boolean = true
  }
  class cons[A](hd: A, tl: => MyStream[A]) extends MyStream[A] {// case class don't allow parameter type => MyStreams[A], but it allows function type  () => MyStreams[A]
    def head = hd

    lazy val tail = tl

    override def isEmpty: Boolean = false
  }

  object cons{//object cons is the company object of class cons( with same name), override apply function so that we can use " cons(h, l) " to create new cons object
  def apply[A](hd: A, tl: => MyStream[A]): MyStream[A] = new cons(hd, tl)//
  }

  def streamRange(lb: Int, hb: Int): MyStream[Int] = {
    if (lb >= hb) `empty`
    else cons(lb, streamRange(lb + 1, hb))
  }

  //here are some of my test
  val a1 = from(0)
  val a2 = a1.take(10).toList
  val a3 = a1.apply(13)
  val a4 =streamRange(1, 5)
  val a5 = streamRange(2, 3)
  val a6 = a4 #++ a5
  val a7 = 1 #:: streamRange(1, 5)
  println("a1: ",a1)
  println("a2:",a2)
  println("a3: ", a3)
  println("test toList: ",a4.toList)
  println("test #::",a7.toList)
  println("test #++:", a6.toList)

}
