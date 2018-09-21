

val r = 1 until 5//range
1 to 5
1 to 10 by 3
6 to 1 by -2

val xs = Array(1, 2, 3, 44)
xs map (x => 2*x) toList//toList can show us the detail
val s = "Hello"
s filter (c => c.isUpper)
s exists (_.isLower)
s forall (_.isUpper)//全部元素成立
val pairs = List(1,2,3 ) zip s//结果长度等于短的
val pairs2 = s zip List(1,2,3)//根据this 的类型，这里s是
//string, 传入参数为List，因此返回的是vector

pairs.unzip//pairs是List，返回两个个list
pairs2.unzip//pairs是vector ，返回两个vector

s map (List('.',_))//本来返回一个元素为list的vector
s flatMap (c => List('.',c))//flatten 后变为String，因为元素都是char
xs max

xs.sum

def scalarProduct(xs:Vector[Double], ys: Vector[Double]):Double = {
  (xs zip ys).map{case (x, y) => x*y}.sum//使用case 需要用花括号
}

def isPrime(n:Int):Boolean = if (n == 2) true
else if (n %2 == 0) false
else{
  (2 to n by 2) forall (d => n%d != 0)
}
isPrime(4)



Map(1 -> 2)
Map("foo" -> "bar")
val m1 = Map((1,2),(3,4))

class SomeClass {

  def update(arg1: Int, arg2: String, arg3: String): String = {

    println("update method called")

    arg1 + "|" + arg2 + "|" + arg3

  }
}


val obj = new SomeClass

val result = (obj(1, "key1") = "Hello")//因为调用了update函数，自然result是string而不是someClass
println(result)

def intNotZero(x:Int): Int = {
  if (x == 0) throw new IllegalArgumentException("fxk")
  else x
}
test("haah"){
  intercept[IllegalArgumentException]{
    isNotZero(0)
  }
}
