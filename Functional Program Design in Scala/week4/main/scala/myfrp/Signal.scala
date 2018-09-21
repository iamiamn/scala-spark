package myfrp


/**
  * Created by LENOVO on 2017/1/2.
  */
class StackableVariable[T](init: T){
  private var values: List[T] = List(init)
  def value: T = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue:: values
    try op finally values = values.tail//只有computeValue时才调用，withValue(this),把自身暂时加入callers的values的头部，在try 完op后的一定执行的finally语句中再取出自己
    //因为当op执行时，遇到sig() = sig() + 1的语句，op会调用sig()，即sig的apply函数，assert过程报错，如果是sig() = sig1() + 1，调用sig1（）则正常，
    //这样同时能避免，追尾赋值，a = b, b = c, c = a的逻辑关系追尾，
    // 假设已经有a() = b(), b(0 = c(),一旦些c() = a()，启动了c的update函数，紧接着启动c的computeValue函数，c把自身加入了自己的obersevant后，运行op：=b()=c()，就
    //报错了。
  }
}
object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()// we need to disable computeValue in NoSignal
}
object Signal {
  val caller = new StackableVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}
//val caller = new StackableVariable[Signal[_]](NoSignal)
class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _  // ()=> T 代表call by name 的T类型值，因为update传入的参数expr: => T, 并且myExpr = () =>expr
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)//别忘了这句

  def getCaller() = caller.value
  def getObs = observers.toString()

  protected  def update(expr: => T): Unit = {
    myExpr = () => expr//expr ： =>T 说明是call by name的引用方式，这里写()=>expr也是继续保持call by name吧
    //在myExpr中并没有对expr进行求值运算，保留了计算公式
    computeValue()
  }
  //!!!假设sig = Signal(expr), sig() 是调用了apply()函数
  //sig() = expr, 则是相当于调用sig.update(expr)的函数

//  protected def computeValue(): Unit = {
//    myValue = caller.withValue(this)(myExpr())
//  }
  protected def computeValue(): Unit = {
  val newValue = caller.withValue(this)(myExpr())
  if (myValue != newValue){
    myValue = newValue//先计算出自己的myValue后，再启动observaer的重新计算，observers包含依赖this值的signal,
    //所以假设场景：依赖状态为b = f(a), c = f(b), 如果b出现改变，则影响不了下层，只会计算出新的b值后，通过b.observer主动去调用c.computeValue
    val obs = observers
    observers = Set()
    obs.foreach(_.computeValue())
  }
}

  def apply(): T = {
    observers += caller.value//所以这个caller是所有共有的,因为其存放在object Signal中，全世界只有一个object signal, 且class signal中导入了object signal了
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

class Var[T](expr: => T) extends Signal[T](expr){//Signal后面必须跟expr
  override def update(expr: => T): Unit = super.update((expr))//调用Signal类的update函数
}
object Var{
  def apply[T](expr: => T) = new Var(expr)
}

