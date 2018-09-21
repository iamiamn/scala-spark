package streams

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b.b2 == goal && (b.b1 == goal)

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
//    def expand(bPairList: List[(Block, Move)]): Stream[(Block, List[Move])] = bPairList match{
//      case Nil => Stream.empty
//      case x::xs => (x._1, x._2 :: history) #:: expand(xs)
//    }
//       expand(b.legalNeighbors)
    //本质就是把legalNeighbors当作Stream来处理，产生一个Stream
    for {
      n <- b.legalNeighbors.toStream//Stream在for循环下，产生的仍然是Stream！！！
    } yield (n._1, n._2 :: history)
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],explored: Set[Block]): Stream[(Block, List[Move])] = {
    //    if (neighbors.isEmpty) Stream.empty
    //    else{
    //for 会自行处理Empty的情况
    for {
      (b, m) <- neighbors
      if (!explored.contains(b))
    } yield (b, m)
  }

    //也可以写成这样
//     for {
//        neighbor <- neighbors
//        if !(explored contains neighbor._1)
//      } yield neighbor
//    }


  /**
   * The function `from` returns the stream of all possible paths
   * that can be followed, starting at the `head` of the `initial`
   * stream.
   *
   * The blocks in the stream `initial` are sorted by ascending path
   * length: the block positions with the shortest paths (length of
   * move list) are at the head of the stream.
   *
   * The parameter `explored` is a set of block positions that have
   * been visited before, on the path to any of the blocks in the
   * stream `initial`. When search reaches a block that has already
   * been explored before, that position should not be included a
   * second time to avoid cycles.
   *
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   *
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if (initial.isEmpty) Stream.empty
    else {
//      val more = initial flatMap (x => newNeighborsOnly(neighborsWithHistory(x._1, x._2), explored))//这个是太多层括号
//      initial ++  from(more, explored ++ (more map (_._1)))//？？？融合stream
      //上面的写法是错误的，++不能用来合并Stream，并且stream本身就没有合并只有head #:: Stream(tail)的操作
      //！！！正确处理方法是处理head元素后，Stream（tail)由新的函数产生
      val neighbors = neighborsWithHistory(initial.head._1, initial.head._2)
      val neighborsNew = newNeighborsOnly(neighbors, explored)
      val expl = explored ++ (neighborsNew map(_._1))
      initial.head #:: from(initial.tail ++ neighborsNew, expl)

    }

  }//！！！注意了这里是flatMap

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, List())), Set(startBlock))

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
//    for {
//      bPair <- pathsFromStart
//      if (done(bPair._1))
//    } yield bPair
    pathsFromStart filter (x => done(x._1))//注意使用for前考虑能否用map filter indexWhere indexOf等代替
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] =
  //pathsToGoal(0)._2//没有考虑结果不存在的情况！！！
  {
    if (pathsToGoal.isEmpty) Nil
    else pathsToGoal.head._2.reverse//take(0)是返回结果的第0个元素
  }
}
