package reductions

import org.scalameter._
import common._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {
  def max(a:Float, b:Float) = if(a < b) b else a

  def lineOfSight(inp: Array[Float],out: Array[Float]): Unit ={
    for {i <- inp.indices}{
      if (i == 0) {out(i) = 0}//！！!注意0
      else {
        out(i) = max(inp(i)/i, out(i - 1))
      }
    }
  }

  sealed abstract class Tree{
    def maxPrevious:Float
  }
  case class Leaf(from: Int, end:Int, maxPrevious: Float) extends Tree
  case class Node(lt: Tree,lr: Tree) extends Tree{
    val maxPrevious = max(lt.maxPrevious,lr.maxPrevious)
  }
  def upsweepSequential(inp:Array[Float], from:Int, end:Int): Float={
    (for(i<-from until end)yield{if (i ==0) 0 else inp(i)/i}).max
  }

  def upsweep(inp: Array[Float], from:Int, end:Int, threshold: Int):Tree = {
    val size = end - from
    if (size < threshold) Leaf(from, end, upsweepSequential(inp,from,end))
    else{
      val mid = (from + end)/2
      val (tl,tr) = parallel(upsweep(inp,from,mid,threshold),
        upsweep(inp,mid,end,threshold))
      Node(tl,tr)
    }
  }

  def downsweepSequential(inp:Array[Float], out:Array[Float], mP:Float, from:Int, end:Int): Unit ={
    var maxAngle = max(mP, inp(from)/from)
    out(from) = maxAngle
    for(i <- from + 1 until end){
      maxAngle = max(inp(i)/i,maxAngle)
      out(i) = maxAngle
    }
  }

  def downsweep(inp:Array[Float], out:Array[Float], maxPrevious: Float, tree:Tree): Unit = tree match{
    case Node(ltree,rtree) =>{
      parallel(downsweep(inp,out,maxPrevious,ltree),
        downsweep(inp,out,max(ltree.maxPrevious, maxPrevious),rtree))
    }
    case Leaf(from,end,_) =>  downsweepSequential(inp,out,maxPrevious,from,end)
  }
  def parLineOfSight(inp:Array[Float], out:Array[Float], threshold:Int): Unit ={
    val tree = upsweep(inp,0,inp.length,threshold)//第0个一直都会是0
    downsweep(inp,out,0,tree)
  }



}