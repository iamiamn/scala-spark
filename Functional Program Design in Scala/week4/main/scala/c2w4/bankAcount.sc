

trait Subscriber{
  def handler(pub: Publisher)
}

trait Publisher{
  private  var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
  subscribers -= subscriber

  def publish():Unit =
    subscribers.foreach(_.handler((this)))
}

class BankAccount extends Publisher{
  private var balance = 0
  def currentBalance: Int = balance // <---make BankAccount a Publisher
  def deposit(amount: Int):Unit = {
    if (amount > 0){
      balance = balance + amount
//      publish() // <---make BankAccount a Publisher
    }
  }

  def withdraw(amount: Int):Unit = {
    if (0<amount && amount <= balance){
      balance = balance - amount
      publish() // <---make BankAccount a Publisher
    }
    else throw new Error("insufficient funds")
  }
}

class Consolidator(observed: List[BankAccount]) extends Subscriber{
  // total 的值是sum()返回
  def sum():Int = observed.map(_.currentBalance).sum
  def handler(pub: Publisher) = sum()
  def totalBalance = total
  private  var total : Int = {this.sum}
//  def totalBalance = sum //修正做法
}

//object observers extends App{
  println("Welcome to the scala worksheet")
  val a = new BankAccount
  val b = new BankAccount
  val c = new Consolidator(List(a,b))
//  c.totalBalance
  a deposit 20
  a.currentBalance
  b.currentBalance

  c.sum//可以计算a.currBalance + b.currBalance的结果
  c.totalBalance//仍然为0？？？为什么会这样,只有把totalBalance 直接改为 = sum才可以修正，这是因为var的懒惰计算？
  b deposit 30
  a.currentBalance
  b.currentBalance
  c.totalBalance
