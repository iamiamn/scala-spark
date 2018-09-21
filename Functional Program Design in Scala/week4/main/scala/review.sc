//sqrt function
def abs(x:Double) = if(x<0) -x else x
def sqrt(x:Int):Double = {
  def genNext(y:Double) = (x/y + y)*1/2
  def isCloseEnough(x:Double, y:Double):Double = {
    if (abs(x - y)/y <= 1e-5) y
    else isCloseEnough(y, genNext(y))
  }
  isCloseEnough(x, 1)
}
sqrt(4)
sqrt(2)
sqrt(1)

//recursive enumeration
//给x元，有可以使用的硬币值列表
def countChange(money:Int, coins:List[Int]):Int = {
  def check(lastMoney:Int, lastCoins:List[Int]):Int = {
    if (lastMoney == lastCoins.head) 1
    else if (lastMoney >= lastCoins.head){
      check(lastMoney - lastCoins.head, lastCoins) + check(lastMoney, lastCoins.tail)
    }
    else 0
  }
  check(money, coins.sorted)
}

def f(a:Int)(b:Int) = {
  a*b
}
val f2 = f(2)(_)//后面必须跟(_)
f2(3)
val ans = f(2)(3)

def divide(x:Int, y:Int): Int ={
  require(y != 0, "denominator can't be zero")
  x / y
}
//divide(3,0)
assert(divide(3,2) == 2,"wrong answer")

