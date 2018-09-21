////package idealize.scala
//abstract class Boolean{
//  def ifThenElse[T](t: => T, e: => T ): T
//  def && (x: => Boolean): Boolean = {
//    ifThenElse( x, false)
//  }
//  def || (x: => Boolean): Boolean = {
//    ifThenElse( true, x)
//  }
//  def unary_! : Boolean          = {
//    ifThenElse( false, true)
//  }
//  def == (x: Boolean):Boolean    = ifThenElse(x, x.unary_!)
//  //unary_!是一个prefix, ||\s 可以做
//  def == (x:Boolean): Boolean    = ifThenElse(!x, x)
//  def < (x: => Boolean): Boolean = ifThenElse(false, x)
//
//}
//  object true extends Boolean {
//  def ifThenElse[T](t: => T, e: => T) = t
//}
//  object false extends Boolean {
//  def ifThenElse[T](t: => T, e: => T) = e
//}

//abstract class Nat{
//  def isZero:Boolean
//  def predecessor: Nat
////  def sucessor : Nat
//  def sucessor: Nat = new Succ(this)//because this is same for both object zero and class succ
//  def + (that: Nat) : Nat
//  def - (that: Nat) : Nat
//
//}
//  object Zero extends Nat{
//    def isZero = true
//    def predecessor = throw new Error("zero has no predecessor")
////    def sucessor = new Succ(Zero)
//    def + (that: Nat) : Nat = that
//    def - (that: Nat) : Nat = {
////      if (that.isZero) Zero//Zero是一个ojbect,不是class不能用new
//      if (that.isZero) this//return this is better
//      else throw new Error("it's negative")
//    }
//  }
//  class Succ(pre : Nat) extends Nat{
//    def isZero = false
////    def predecessor = {
////      if (pre.isZero) Zero
////      else (this - new Succ(Zero))
////    }
//    def predecessor = pre// as simple as it can be
////    def sucessor = new Succ(this)
////    def + (that: Nat) : Nat = {
////      if (that.isZero) new Succ(this.predecessor)
////      else (new Succ(this) + that.predecessor)
////    }
//    def +(that: Nat):Nat = new Succ(pre + that)
//
//    //!!!need to let it touch zero by recursive
//    // so we should use succ(this)
//    override def -(that: Nat): Nat = {
//      if (that.isZero) this
////      else (this.predecessor - that.predecessor)
//      else new Succ(pre - that.predecessor)//here should use new??? may first way can work
//    }
//  }
//trait List[T] {
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//}
//class Cons[T](val head: T, val tail : List[T]) extends List[T]{
//  def isEmpty = false
//}
//class Nil[T] extends List[T]{
//  def isEmpty = true
//  def head: Nothing = throw new NoSuchElementException("Nil.head")
//  def tail:Nothing = throw new NoSuchElementException("Nil.tail")
//}
//object List{
//  //List(1,2) = List.apply(1, 2)
//  def apply[T](x1 : T, x2 : T): List[T] = new Cons(x1, new Cons(x2, new Nil))
//  def apply[T]() = new Nil
//}
//List(1, 2)
//

trait Expr{
  def eval: Int = this match{
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}
Sum(Number(1), Number(2)) match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}
//so you can write Number(1) instead of new Number(1).
//these classes are empty if u don't define eval function in trait
object exprs{
  def show(e:Expr):String = e match{
    case Number(x) => x.toString
    case Sum(l, r) => show (l) + " + " +show(r)
  }
  show(Sum(Number(1), Number(44)))
}










