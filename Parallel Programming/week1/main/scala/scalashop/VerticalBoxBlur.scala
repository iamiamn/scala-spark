package scalashop
import common._
import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 5,//测试运行一共测试5次
    Key.verbose -> true//false 仍然输出GC detected，但会少了很多
    //系统信息，比如最后的所有benchrun的统计
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)//这里为什么可以缺少array
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for {
      currX <- from until end
      currY <- 0 until src.height
      if (currX >= 0 && currX < src.width)
    } yield{dst(currX, currY) = boxBlurKernel(src,currX, currY,radius)}
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val interval:Int = if (numTasks >= src.width) 1 else ((src.width - 1)/numTasks + 1)
    //numTasks 过多的时候确保不会报错
    val tasks = for (currStart <- 0 until src.width by interval)
      yield task{blur(src, dst, currStart,currStart + interval, radius)}
    tasks.map(t => t.join())//join是确保该task完成，要保证每个计算都有结果了
  }

}
