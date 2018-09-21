/**
  * Created by LENOVO on 2017/1/15.
  */
package lecture
import org.scalameter._
import common._
import Math.pow
object MergeSort {
  //a bit of reflection to access the private sort1 method, which takes an
  //offset and an argument
  private val sort1 = {
    val method = scala.util.Sorting.getClass.getDeclaredMethod("sort1", classOf[Array[Int]], classOf[Int], classOf[Int])
    method.setAccessible(true)
    (xs: Array[Int], offset: Int, len: Int) => {
      method.invoke(xs, offset.asInstanceOf[AnyRef], len.asInstanceOf[AnyRef])
    }
  }

  def quickSort(xs: Array[Int], offset: Int, length: Int) = {
    sort1(xs, offset, length)
  }

  @volatile var dummy: AnyRef = null

  def parMergeSort(xs: Array[Int], maxDepth: Int) = {
    val ys = new Array[Int](xs.length) //allocate a helper array
    dummy = ys //???why

    def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
      var left = from
      var right = mid
      var i = from
      while (left < mid && right < until) {
        while (left < mid && src(left) <= src(right)) {
          dst(i) = src(left)
          i += 1
          left += 1
        }
        while (right < until && src(right) <= src(left)) {
          dst(i) = src(right)
          i += 1
          right += 1
        }
      }
      while (left < mid) {
        dst(i) = src(left)
        i += 1
        left += 1
      }
      while (right < mid) {
        dst(i) = src(right)
        i += 1
        right += 1
      }
    }
    //merging two sorted array
    def sort(from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        quickSort(xs, from, until - from)
      } else {
        val mid:Int = (from + until) / 2
        val right = task {
          sort(mid, until, depth + 1)
        }
        sort(from, mid, depth + 1)
        right.join

        val flip = (maxDepth - depth) % 2 == 0
        val src = if (flip) ys else xs
        val dst = if (flip) ys else ys
        merge(src, dst, from, mid, until)
        //!!!非常高级的做法，层层往上merge的时候需要将底层完成merge的结果拷贝到临时的数组
        //就在ys和xs两个数组间互相拷贝节省开辟数组
      }
    }
    sort(0, xs.length, 0) //???

    // 3) In parallel, copy the elements back into the source array.
    // Executed sequentially, this step takes:
    // 􀀀 ~23x less time than a full quickSort without GCs (best time)
    // 􀀀 ~16x less time than a full quickSort with GCs (average time)
    // There is a small potential gain in parallelizing copying.
    // However, most Intel processors have a dual􀀀channel memory controller,
    // so parallel copying has very small performance benets.
    def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        Array.copy(src, from, target, from, until - from)
      } else {
        val mid:Int = (from + until) / 2
        val right = task {
          copy(src, target, mid, until, depth + 1)
        }
        copy(src, target, from, mid, depth + 1)
        right.join
      }
    }
    copy(ys, xs, 0, xs.length, 0)
  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def initialize(xs: Array[Int]): Unit = {
    //初始化一个xs
    var i = 0
    while (i < xs.length) {
      xs(i) = i % 100
      i += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val maxDepth = 7
    val xs = new Array[Int](length)
    val seqtime = standardConfig setUp {
      _ => initialize(xs)
    } measure {
      quickSort(xs, 0, xs.length)
    }
    println(s"sequntial sum time: $seqtime ms")

    val partime = standardConfig setUp {
      _ => initialize(xs)
    } measure {
      parMergeSort(xs, maxDepth)
    }
    println(s"fork/join time: ${partime}ms")
    println(s"seppdup: ${seqtime / partime}")
  }

}

object dataOperation{
  val l1 = List(1,2,3,4)
  val f = (x:Int) => x*x
  val l2 = l1.map(f.compose(_ + 1))
  println(l2)

  //List version, O(n) complexity
  def mapSeq[A, B](lst: List[A], f: A => B): List[B] = lst match{
    case Nil => Nil
    case h :: t => f(h) :: mapSeq(t, f)
  }

  def mapASegSeq[A, B](inp: Array[A], left: Int, right:Int, f: A=>B,
                       out: Array[B]): Unit = {
    var i = left
    while(i < right){
      out(i) = f(inp(i))
      i += 1
    }
  }
  private var threshold = 10000
  def mapASegPar[A, B](inp: Array[A], left: Int, right:Int, f: A => B,
                       out: Array[B]):Unit = {
    if ((right - left) < threshold){
      mapASegSeq(inp, left, right, f, out)
    } else{
      val mid:Int = (left + right)/2
      parallel(mapASegPar(inp, left, mid, f, out),
        mapASegPar(inp, mid, right,f, out))

    }
  }

  def normsOf(inp: Array[Int], p: Double, left: Int, right: Int,
              out: Array[Double]): Unit = {
    var i = left
    while (i < right){
      out(i) = pow(inp(i), p)
      i = i + 1
    }
  }

  def normsOfPar(inp: Array[Int], p: Double, left: Int, right:Int,
                 out: Array[Double]): Unit ={
    if ((right - left) < threshold){
      normsOf(inp,p,left,right,out)
    }else{
      val mid:Int = (left + right)/2
      parallel(normsOfPar(inp, p, left, mid, out),
        normsOfPar(inp, p, mid, right, out))
    }
  }
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 15,
    Key.exec.maxWarmupRuns -> 30,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def initialize(xs: Array[Int]): Unit = {
    //初始化一个xs
    var i = 0
    while (i < xs.length) {
      xs(i) = i % 100
      i += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val length = 2000000
    val xs = new Array[Int](length)
    val ys = new Array[Int](length)
    var zs = new Array[Double](length)
    val info = for (threshold <- (10000 until 100000 by 5000))yield{
//      val f = (x:Int) => x/2+x/3
//      val seqtime = standardConfig setUp {
//        _ => initialize(xs)
//      } measure {
//        mapASegSeq(xs,0,length,f,ys)
//      }
//
//      val partime = standardConfig setUp {
//        _ => initialize(xs)
//      } measure {
//        mapASegPar(xs,0,length,f,ys)
//      }
      val zs = new Array[Double](length)
      val seqtime2 = standardConfig setUp{
        _ => initialize(xs)
      }measure{
        normsOf(xs, 3.5,0,length,zs)
      }
      val partime2 = standardConfig setUp{
        _ => initialize(xs)
      } measure{
        normsOfPar(xs, 3.5, 0, length, zs)
      }
      val speedup = seqtime2/partime2
      List(threshold, seqtime2, partime2, speedup).reverse
    }
    println(info)
//    println("mapASeg")
//    println(s"sequntial sum time: $seqtime ms")
//    println(s"fork/join time: ${partime}ms")
//    println(s"seppdup: ${seqtime / partime}")
//    println("norms")
//    println(s"sequntial sum time: $seqtime2 ms")
//    println(s"fork/join time: $partime2 ms")
//    println(s"speedup: ${seqtime2/partime2} ms")

  }

}

object ScanOperation{
  sealed abstract class TreeResA[A]{val res:A}//这里定义了val,后面case class 虽然可以在参数列表中
  //直接定义成员val,但是需要override
  case class Leaf[A](from: Int, to:Int, override val res: A) extends TreeResA[A]
  case class Node[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

  var threshold:Int = 1

  def reduceSeg1[A](inp: Array[A], from: Int, to:Int,a0:A,  f: (A, A) => A):A = {
    var a = a0
    var i = from
    while (i < to){
      a = f(a,inp(i))
      i += 1
    }
    a
  }

  def upsweep[A](inp: Array[A], from:Int, to:Int, f: (A,A)=>A):TreeResA[A] = {
    if (to - from < threshold) Leaf(from,to,reduceSeg1(inp,from + 1,to, inp(from),f))
    else{
      val mid = from + (to - from)/2
      val (tL,tR) = parallel(upsweep(inp,from,mid,f), upsweep(inp,mid, to, f))
      Node[A](tL, f(tL.res,tR.res), tR)
    }
  }

  def scanLeftSeg1[A](inp:Array[A], from:Int, to:Int, a0:A, f: (A, A)=>A, out:Array[A]): Unit ={
    var a = a0
    var i = from
    while (i < to){
      a = f(a0,inp(i))
      out(i) = a
      i += 1
    }
  }
  def downsweep[A](inp: Array[A], a0: A, tree: TreeResA[A], f:(A,A)=>A, out:Array[A]): Unit = tree match{
      //!!!missing parameter type for expanded function, I forget to write down "tree match"
    case Leaf(from, to, res) => scanLeftSeg1(inp, from, to, a0, f, out)
    case Node(tL, res, tR) => {
      parallel(downsweep(inp, a0, tL, f, out),
        downsweep(inp, f(a0,tL.res),tR,f,out))
    }
  }

  def scanLeft[A](inp:Array[A], out:Array[A], f:(A,A)=>A, a0:A): Unit ={
    val tr = upsweep(inp,0,inp.length,f)
    downsweep(inp,a0,tr,f,out)
  }
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 30,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer( new Warmer.Default)
  def main(args: Array[String]): Unit = {
//    val suList = for (length <- 100000 until 10000000 by 1000000) yield {
    val suList = for(i <- 1 until 5) yield{
      threshold = ((for (_ <- 1 until i) yield 10)foldLeft(10))(_*_)
      val a0 = 100
      val length = 1000000
      val f = (x: Int, y: Int) => x + y
      val inpArray = (0 until length).toArray
      val outArray = new Array[Int](length)

      val seqTime = standardConfig measure {
        scanLeftSeg1(inpArray, 0, length, a0, f, outArray)
      }
      val parTime = standardConfig measure {
        scanLeft(inpArray, outArray, f, a0)
      }
      println(s"sequentail time: $seqTime ms")
      println(s"fork/join time: $parTime ms")
      println(s"speedup: ${seqTime / parTime}")
      seqTime/parTime
    }
    println(suList)
  }
}