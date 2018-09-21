object mergeSort{
  def merge(xl: List[Int], yl: List[Int]): List[Int] = (xl, yl) match {
    case (Nil, yl) => yl
    case (xl, Nil) => xl
    case (x :: xt, y :: yt) =>
      if (x < y) x :: merge(xt, yl)
      else y :: merge(xl, yt)
  }

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      merge(msort(xs.take(n)), msort(xs.drop(n)))
    }
  }
}
println(mergeSort.msort(List(1,334,4, 5,6, 7,3, 4)))
