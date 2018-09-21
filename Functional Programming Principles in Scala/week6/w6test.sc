object test{
  val xs = Array(1, 2, 3, 44)
  xs map (x => x*2)
  val s = "Hello World"
  s filter (c => c.isUpper)
  s exists (c => c.isUpper)
  s forall (c => c.isUpper)
  val pairs = List(1, 2, 3) zip s
  pairs.unzip
  s flatMap (c=> List('.', c))
  xs.sum
  xs.max
//  To list all combinations of numbers x and y where x is drawn from
//    1..M and y is drawn from 1..N:
  val a = (1 to 3) flatMap ( x=> (4 to 6) map(y => (x,y)))
  println(a)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs zip ys).map(xy => xy._1 + xy._2).sum
  }
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs zip ys).map{ case (x,y) => x*y}.sum
  }
  scalarProduct(Vector(1, 2, 3), Vector(4,5, 6))
//  scalarProduct2(1 to 3, 4 to 6)
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

}

object class2{
  def scalarProduct(xs: List[Double], ys: List[Double]):Double = {
    val a = for {
      (i, j) <- xs zip ys
    }yield i*j
    a.sum
  }
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
  val t1 = for {
    i <- 1 until 4
    j <- 4 until 7
    if isPrime(i + j)
  } yield (i, j)
  val t2 = ((1 until 4) map ( x => (1 until x) map (j => (x, j)) )).flatten
  val t3 = (1 until 4) flatMap ( i => (1 to i) map ( j => (i, j)))

}
println(class2.t2)
println(class2.t3)
print(List("a", "b", "c") mkString "\n")

object class4{
  class Poly(val terms: Map[Int, Double]){
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
//    def adjust(term: (Int, Double)): (Int, Double) = {
//      for {
//        (_key, _value) <- terms
//        if _key == term._1
//      } yield (term._1, term._2 + _value)
//    }//这个写法会导致结果是Map[Int, Double]类型
    def adjust(term: (Int, Double)): (Int, Double) = {
  val (exp, coeff) = term
  terms get exp match {//get 操作为取键，可能为None or Some(value)
    case Some(coeff1) => exp -> (coeff + coeff1) // ->取value的引用可用于修改
    case None => exp -> coeff
  }
}
  override def toString =
    (for {
      (exp, coeff) <- terms.toList.sorted.reverse
    } yield coeff + "x^" + exp) mkString "+"

  }
  val p1 = new Poly(Map(1 -> 2, 3 -> 4, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3, 3 -> 7))
  val p3 = p1 + p2
}
println(class4.p3)

object class42{
  class Poly(val terms0: Map[Int, Double]){
    def this(bindings: (Int, Double)*) = this(bindings.toMap)//*means repeated
    val terms = terms0 withDefaultValue 0.0//here use 0 will turn wrong since the default is not double
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    //    def adjust(term: (Int, Double)): (Int, Double) = {
    //      for {
    //        (_key, _value) <- terms
    //        if _key == term._1
    //      } yield (term._1, term._2 + _value)
    //    }//这个写法会导致结果是Map[Int, Double]类型
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
       exp -> (coeff + terms(exp))

    }
    override def toString =
      (for {
        (exp, coeff) <- terms.toList.sorted.reverse
      } yield coeff + "x^" + exp) mkString "+"

  }
  val p1 = new Poly(1 -> 2, 3 -> 4, 5 -> 6.2)
  val p2 = new Poly(0 -> 3, 3 -> 7)
  val p3 = p1 + p2
}
object class43{
  class Poly(val terms0: Map[Int, Double]){
    def this(bindings: (Int, Double)*) = this(bindings.toMap)//*means repeated
    val terms = terms0 withDefaultValue 0.0//here use 0 will turn wrong since the default is not double
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))//default is original this.terms

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
//      terms(term._1) -> (terms(term._1) + term._2)
      val (exp, coeff) = term
      val changed = coeff + terms(exp)

//      exp -> (changed)// 如果出现了terms中没有的键：exp, 这个键其实不会被加入，
//      //这个 a->b的语句在adjust能起作用是因为返的是（Int, Double)，这语句是个返回值
      terms + (exp -> (changed))//这样才能起修改作用

      terms
    }
//    def adjust(term: (Int, Double)): (Int, Double) = {
//      val (exp, coeff) = term
//      exp -> (coeff + terms(exp))
//
//    }
    override def toString =
      (for {
        (exp, coeff) <- terms.toList.sorted.reverse
      } yield coeff + "x^" + exp) mkString "+"

  }
  val p1 = new Poly(1 -> 2, 3 -> 4, 5 -> 6.2)
  val p2 = new Poly(0 -> 3, 3 -> 7)
  val p3 = p1 + p2
}
print(class43.p3)

val t3 = (1 until 4) flatMap ( i => (1 to i) map ( j => (i, j)))
val t = (1 to 4) map (x => x*x)
val t1 = 1 :: 2 :: List(3)
val t2 = (List(('a',1),('b',2)).toSet[(Char,Int)].subsets map (_.toList)).toList
val t4 = Map("1" -> 2, "2" -> 3).toList //map2list是(k, v)
val t5 = List(List(5),List(List(List(3,5), 4),5))//必须成员全部是List
//val t5 = List(List(5), List(3, 4))
val t6 = t5.flatten
