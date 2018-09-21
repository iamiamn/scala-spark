/**
  * Created by LENOVO on 2016/12/13.
  */
//object streamDefine {
//  abstract class MyStream[+A]{
//    def isEmpty:Boolean
//    def head:A
//    def tail :=> MyStream[A]
//  }
//  case class Cons[T](head: T, tail:=> MyStream[T]) extends MyStream[T]{
//    def isEmpty = false
//  }
//  case object Empty extends MyStream[Nothing]{
//    def isEmpty = true
//    def head = throw new NoSuchElementException("empty.head")
//    def tail = throw new NoSuchElementException("empty.tail")
//  }
//
//  def take(n:Int):Stream[Double] = {
//    if (n == 0) this
//    else this.head #:: this.tail.take(n - 1)
//  }
//
//}

object MyStream extends App{
//  trait MyStream[+A] extends Seq[A]{

  abstract class MyStream[+A]{//这里定义为trait或abstract class 均可，但是如果extends Seq[A]，则需要
    //在head isEmpty等def左边写上override
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]
    def filter(p : A => Boolean): MyStream[A] = {
      if(isEmpty) this
      else if (p(head)) cons(head, tail.filter(p))
      else tail.filter(p)
    }
    def streamRange(lb:Int, hb:Int):MyStream[Int] = {
      print(lb + "")
      if (lb >= hb) MyStream.empty
      else MyStream.cons(lb, streamRange(lb + 1, hb))

    }
    def apply(n :Int):A = {
      if (n == 0) head
      else tail.apply(n - 1)
    }
//    def #:: [T] (n : T, myStream: MyStream[T]) = MyStream.cons(n, myStream)
    def take(n : Int): MyStream[A] = {
      if (n == 0) this
      else cons(head,  tail.take(n - 1))
    }
    def toList(num:Int):List[Any] = {//有没有方法可以判断到底计算到那里
      def getList(list:List[Any], n : Int) : List[Any]= {
        if (n == 0) list
        else getList(this.apply(n) :: list, n - 1)//！！！如果不改成List【Any】，会因为编译器判断this.apply(n)的类型不明确
        //因此类型检查不通过
      }
       getList(Nil, num)

    }
  }

//  def from(n:Int):MyStream[Int] = n #:: from(n + 1)
  def from(n:Int):MyStream[Int] = cons(n, from(n + 1))

  def cons[T](hd: T, tl: => MyStream[T]) = new MyStream[T] {//使用new MyStream需要先定义了trait MyStream
    def isEmpty = false
    def head = hd
    lazy val tail = tl//cons为lazy value，被调用时才会计算，且保存计算结果
  }
  val empty = new MyStream[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("empty.head")
    def tail = throw new NoSuchElementException("empty.tail")
  }
  val a1 = from(0)
  val a2 = a1.take(10).toList(10)//？？？不知道怎么写toList函数
  val a3 = a1.apply(10)
  println(a1,a2,a3)
}
