package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("order") = forAll { (a:Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("empty") = forAll { a:Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("melding") = forAll { (a:Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    val merged = meld(h, i)

    findMin(merged) == (if (a > b) b else a)
  }

  property("melding heaps") = forAll { (h:H, i:H) =>
    val minH = findMin(h)
    val minI = findMin(i)
    val merged = meld(h, i)
    findMin(merged) == (if (minH < minI) minH else minI)
  }

  property("associative meld") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    toList(a) == toList(b)
  }

  property("order of mins") = forAll { (h:H) =>
    toList(h).zip(toList(h).drop(1)).forall {
      case (x, y) => x <= y
    }
  }
  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, empty), (9, genHeap))
  } yield insert(n, h)

//  lazy val genHeap: Gen[H] = for {
//    x <- arbitrary[Int]
//    h <- oneOf(List(),genHeap)
//  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
}