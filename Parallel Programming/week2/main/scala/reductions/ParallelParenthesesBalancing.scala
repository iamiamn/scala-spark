package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}
//parallel balancing time: 5.943993375 ms
//speedup: 329.0452088660008
//sequential result = true
//sequential balancing time: 1955.8425415749998 ms
//理论加速量级应该在
//10000000/(math.log(10000000/10000)/math.log(2) * 10000)
//月等于100




object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
//  def balance(chars: Array[Char]): Boolean = {
//    def balOrNot(stack:Queue[Int], balChars:Array[Char]): Queue[Int]=balChars match{
//      case Array() => stack
//      case _ => {
//        if (chars.head == '(') stack.enqueue(1)
//        else if (chars.head == ')') {
//          if (stack.size == 0) {stack.enqueue(1);return stack}
//          else stack.dequeue
//        }
//        balOrNot(stack, balChars.tail)
//      }
//    }
//    balOrNot(new Queue[Int], chars).isEmpty
//  }
  def balance(chars: Array[Char]): Boolean = {
    def code(ch: Char): Int = ch match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }
    def loop(chLs: List[Char], acc: Int = 0): Int = chLs match {
      case head::tail if acc >= 0 => loop(tail, acc + code(head))
      case _ => acc
    }
    loop(chars.toList) == 0
  }//这种方法如果长序列中的左部出现了stack(')')的情况，无法及早停掉程序，会一直从左跑到右



  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) :(Int,Int)/*: ???*/ = {
      if (idx < until){
        chars(idx) match{
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' => {
            if (arg1 > 0) traverse(idx + 1, until, arg1- 1, arg2)
            else traverse(idx + 1, until, arg1, arg2 + 1)
          }//只要出现了（ 或者),都会对arg1，arg2改变，不像balance函数，会忽略arg<=0的情况下出现的)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }else (arg1, arg2)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      val size = until - from
      if (size > threshold){
        val halfsize = size/2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, from + halfsize),
          reduce(from + halfsize, until))
        if (a1 > b2){
          ((a1 - b2 + b1), a2)//!!!!
        }
        else {
          (b1, b2 - a1 + a2 )
        }
      }
      else{
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
