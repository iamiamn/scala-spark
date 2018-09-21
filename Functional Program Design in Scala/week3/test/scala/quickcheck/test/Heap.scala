package quickcheck.test

// Figure 3, page 7
trait BinomialHeap extends quickcheck.Heap {

  type Rank = Int
  case class Node(x: A, r: Rank, c: List[Node])
  override type H = List[Node]//heap放着rank不同的Node

  protected def root(t: Node) = t.x
  protected def rank(t: Node) = t.r
  protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r,两颗树的rank相同，做link操作，取顶点小的作为rank 为t1.r + 1的新树的顶点
    if (ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t2::t1.c) else Node(t2.x, t2.r+1, t1::t2.c)
  protected def ins(t: Node, ts: H): H = ts match {
    case Nil => List(t)
    case tp::ts =>
      if (t.r<tp.r) t::tp::ts else ins(link(t, tp), ts) // t.r<=tp.r，这是由meld保证的
  }

  override def empty = Nil
  override def isEmpty(ts: H) = ts.isEmpty

  override def insert(x: A, ts: H) = ins(Node(x,0,Nil), ts)
  override def meld(ts1: H, ts2: H): H = (ts1, ts2) match {
    case (Nil, ts) => ts
    case (ts, Nil) => ts
    case (t1::ts1, t2::ts2) =>
      if (t1.r<t2.r) t1::meld(ts1,t2::ts2)
      else if (t2.r<t1.r) t2::meld(t1::ts1,ts2)
      else ins(link(t1,t2),meld(ts1,ts2))
    //可能出现link(t1,t2)产生的新树rank与meld结果中相同，或者rank比meld结果小，不可能比meld大，
      //因为t1::ts1 组成的Heap，Heap中的Node排列顺序是rank从小到大
  }

  override def findMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t::Nil => root(t)
    case t::ts1 =>
      val x = findMin(ts1)
      if (ord.lteq(root(t),x)) root(t) else x
  }
  override def deleteMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t::ts =>
      def getMin(t: Node, ts: H): (Node, H) = ts match {
        case Nil => (t, Nil)
        case tp::tsp =>
          val (tq,tsq) = getMin(tp, tsp)
          if (ord.lteq(root(t),root(tq))) (t,ts) else (tq,t::tsq)
      }
      val (Node(_,_,c),tsq) = getMin(t, ts)//找到Heap中root最小的Node，把node的顶点去除，node剩下的C和tsq融合
      meld(c.reverse, tsq)
  }
}

trait Bogus1BinomialHeap extends BinomialHeap {
  override def findMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t::ts => root(t)//ts的Node按照从小到大排列
  }
}

trait Bogus2BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (!ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t2::t1.c) else Node(t2.x, t2.r+1, t1::t2.c)
  //顶点比子树的顶点都大？但是单独改这个不够
}

trait Bogus3BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.r==t2.r
    if (ord.lteq(t1.x,t2.x)) Node(t1.x, t1.r+1, t1::t1.c) else Node(t2.x, t2.r+1, t2::t2.c)
}

trait Bogus4BinomialHeap extends BinomialHeap {
  override def deleteMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t::ts => meld(t.c.reverse, ts)
  }
}

trait Bogus5BinomialHeap extends BinomialHeap {
  override def meld(ts1: H, ts2: H) = ts1 match {
    case Nil => ts2
    case t1::ts1 => List(Node(t1.x, t1.r, ts1++ts2))
  }
}
