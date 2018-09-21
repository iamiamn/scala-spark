class BankAccount{
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount >0) balance = balance + amount
  }
  def withdraw(amount: Int):Int =
    if (0 < amount && amount <= balance){
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
}

val x = new BankAccount
val y = x
val z = new BankAccount
def f(a: BankAccount, b : BankAccount): (Int, Int) = {
  a.deposit(50)
  b.deposit(50)
  (a.withdraw(10), b.withdraw(10))
}
f(x,z)
f(x,y)//this suggests that y is another name for x