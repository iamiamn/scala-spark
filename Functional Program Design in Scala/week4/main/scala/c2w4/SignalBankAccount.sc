package calculator
import Signal._


class BankAccout{
  val balance = Var(0)
  def deposit(amount: Int): Unit =
    if (amount > 0){
      val b = balance()
      balance() = balance() + amount
    }
  def withdraw(amount: Int):Unit =
    if (amount > 0 && amount <= balance()){
    val b = balance()
    balance() = b - amount
    } else throw new Error("Insufficient funds")
}

def consolidated(accts: List[BankAccout]): Signal[Int] =
  Signal(accts.map(_.balance).sum)

