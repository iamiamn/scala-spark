
///*one solution*/
//trait Expr
//case class Number(n: Int) extends Expr
//case class Sum(e1: Expr, e2: Expr) extends Expr
////下面的如果不注释，会导致apply函数重复定义，因为前面的case class
////object Number{
////  def apply(n: Int) = new Number(n)
////}
////object Sum{
////  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
////}
////不定义case class ,只用object 是不能使用下面的match
//def eval(e: Expr): Int = e match{
//  case Number(n) => n
//  case Sum(e1, e2) => eval(e1) + eval(e2)
//}
//def show(e:Expr): String = e match{
//  case Number(n) => n.toString()
//  case Sum(e1, e2) => show(e1) + " + " + show(e2)
//}
//eval(Sum(Number(1), Number(2)))
////eval(Number(Sum(Number(1), Number(2))))//Number只能传入int
//eval(Number(1)) + eval(Number(2))
//show(Sum(Number(2), Number(2)))

/*second solution*/
trait Expr{
  def eval:Int = this match{
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval*e2.eval
      //why can't sum work like this:
      //case Sum(e1, e2) => eval(e1) + eval(e2)
      //becasue this is this.eval(e1), but eval does not
      //take any parameter
  }
  def show: String = this match {
    case Number(n) => n.toString()
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(e1, e2) => e1 match{
      case Sum(_, _) => "(" + e1.show + ")*" + e2.show
      case Number(n) => n.toString() + e2.show
    }
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

Number(1).eval + Number(2).eval
//show(Number(1).eval + Number(2).eval)// this can't work becasue
//parameter is Int
(Sum(Number(2), Number(2))).show
(Prod(Sum(Number(1), Number(2)), Number(1))).show

//println(Nil.head)// 找不到该元素
println(Nil.isEmpty)
println(List(Nil).isEmpty)
println(List(Nil).head)
println(List(Nil).tail)
println(List(Nil).head.isEmpty)
println(List(Nil).tail.isEmpty)