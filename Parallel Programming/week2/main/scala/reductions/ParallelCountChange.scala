package reductions

import common._
import org.scalameter._


object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Double = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
      seqtime/fjtime
    }

//    val speedUpList = Array(measureParallelCountChange(ParallelCountChange.moneyThreshold(amount)),
//    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length)),
//    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins.length)))
//    println(speedUpList)
//    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
//    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins.length))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else coins match {
        case List() => 0
        case xh :: xt => {
          if (money < xh) 0
          else if (money == xh) 1
          else {
            countChange(money - xh, coins) +
              countChange(money, xt)
          }
        } //注意考虑coins没有的情况
      }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = coins match{
    case List() => if (money == 0) 1 else 0
    case xh::xt=> {
      if (money < xh) 0
      else if (money == xh) 0//必须加上money的判断，否则只靠totalCoins的判断的threshold函数，会陷入死循环！！！
      else if(threshold(money, coins)) countChange(money, coins)
      else {
        val (c1, c2) = parallel(parCountChange(money - xh, coins, threshold),
          parCountChange(money, xt, threshold))
        c1 + c2
      }
    }

  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    def func(moneyAmount: Int, coinsList: List[Int]):Boolean ={
      if (2*startingMoney/3 >= moneyAmount) true else false
      //!!!we need to avoid 2/3 = 0，so we use 2*x/3 instead of 2/3*x
    }
    func
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold ={
    def func(money:Int, coins:List[Int]):Boolean={
      if (coins.length <= 2*totalCoins/3) true else false
    }
    func
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, totalCoins:Int): Threshold = {
    def func(money:Int, coins:List[Int]):Boolean = {
      if (money*coins.length <= startingMoney*totalCoins/2) true else false
    }
    func
  }
}
