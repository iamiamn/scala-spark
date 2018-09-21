def init[T](xs: List[T]): List[T] = xs match{
  case List() => throw new Error("init of empty list")
  case List(x) => Nil//List()
  case y :: ys => y :: init(ys)//这里case 的排列顺序起了作用，保证这里ys.length>= 1
}

def concat[T](xs: List[T], ys: List[T]):List[T] = xs match{
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}
def removeAt[T](n: Int, xs: List[T]):List[T] = xs match{
  case List() => throw new Error(" out of bound")
  case y :: ys => {
    if (n > 0) y :: removeAt(n - 1, ys)
    else ys
  }
}

def flatten(xs: List[Any]): List[Any] = xs match{
//  case List(x) => List(x)
  case List() => List()
  case y :: ys => y match{
//    case z :: zs => (z :: flatten(zs)) ++ flatten(ys)//这里括号内应该换成flatten(y) 但无法编译！！！
    case z :: zs => flatten(z :: zs) ++ flatten(ys)//flatten(y)无反应就换成这样
    case x => x :: flatten(ys)//z :: zs在前才能解决问题

  }
}

val a = List(1, 2, 4, 6)
println(init(a))
val b = List(3)
b.head
b.tail//res2: List[Int] = List() // 类型仍然是List[Int]，只不过是空
removeAt(2, a)
println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
val d = flatten(List(List(1, 1), 2, List(List(List(4, 1), 2), List(5, List(7,8)))))

object mergeSort{
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xl: List[Int], yl: List[Int]): List[Int] = (xl, yl) match {
        case (Nil, yl) => yl
        case (xl, Nil) => xl
        case (x :: xt, y :: yt) =>
          if (x < y) x :: merge(xt, yl)
          else y :: merge(xl, yt)
      }
      merge(msort(xs.take(n)), msort(xs.drop(n)))
    }
  }
}
println(mergeSort.msort(List(1,334,4, 5,6, 7,3, 4)))
import math.Ordering
object mergeSortT{
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xl: List[T], yl: List[T]): List[T] = (xl, yl) match {
        case (Nil, yl) => yl
        case (xl, Nil) => xl
        case (x :: xt, y :: yt) =>
          if (ord.lt(x, y)) x :: merge(xt, yl)
          else y :: merge(xl, yt)
      }
      merge(msort(xs.take(n)), msort(xs.drop(n)))
    }
  }
}
//mergeSortT.msort(List("a", "b", "c", 1, "ab"))//无法通过implicite编译
mergeSortT.msort(List("a", "b", "c", "dd", "ab"))
//parameter used in msort have different types, one of them is function type,
//compiler can infer the type of x and y from function type
mergeSortT.msort(List(1, 3, 4, 5, 6, 6, 9))
//
//abstract class List[T] { ...
//  def map[U](f: T => U): List[U] = this match {
//    case List() => this
//    case x :: xs => f(x) :: xs.map(f)
//  }
//}

def squareList(xs: List[Int]): List[Int] = xs match{
  case Nil => Nil
  case y :: ys => y*y :: squareList(ys)
}
//def squareList2(xs: List[Int]): List[Int] = xs map ((x: Int): Int => x*x)
def squareList2(xs: List[Int]): List[Int] = xs map (x => x*x)

squareList(a)
squareList2(a)

def pack[T](xs: List[T]): List[Any] = xs match{
  case Nil => Nil
  case y :: ys => y match{
    case z :: zs => {
//      println(ys.isEmpty, " - ", z)
      if (!(ys.isEmpty)  && (z == ys.head)) {
//        println((ys.head :: (z :: zs)))
        pack((ys.head :: (z :: zs)) :: ys.tail)
      }
      else (z :: zs) :: pack(ys)//key is use :: between two element，then left part will be a single element
      //if use ++ , then left part will be expended.
    }
    case z => {
//      println( z)
      pack(List(z) :: ys)
    }
  }
}

def pack2[T](xs: List[T]): List[Any] = xs match{
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs.span(y => y == x)
    first :: pack2(rest)
  }

}

def encode[T](xs: List[T]): List[Any] = xs match{
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs.span(y => y == x)
    (x, first.length) :: encode(rest)
  }

}

pack(List("a", "a", "a", "b", "c", "c", "a"))
encode(List("a", "a", "a", "b", "c", "c", "a"))

///class 5.5
def sum1(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)
def sum(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
sum(List())
sum1(List())//???why can this run without error when input empty list,

//def concatFR[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys) (_ :: _)
def concatFR[T](xs: List[T], ys: List[T]): List[T] = (xs foldLeft ys) ((x, y) => (y :: x))

concatFR(List(1,2,3), List(4,5,6))

///class 5.5 test
def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldRight List[U]())(f(_) :: _)
def lengthFun[T](xs: List[T]): Int = (xs foldRight 0) ((x, y) => (y + 1))
lengthFun(List(1,2,3))
def op(x: Int):Int = x + 3
mapFun(List(1, 2, 3), op)//u can't use (x => x + 1) to replace op ,since type U is not defined in mapFun
//complier don;t know whether plus operation can work or not




