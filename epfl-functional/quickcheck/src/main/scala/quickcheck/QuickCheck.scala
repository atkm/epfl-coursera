package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
    )
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("ins1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(a, insert(a, empty)))
    hsort(insert(b, insert(b, h))) == List(a,a,a,b,b).sorted
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    val h1 = insert(a, insert(b, empty))
    val h2 = insert(b, insert(a, empty))
    findMin(h2) == min && findMin(h1) == min
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h)) == true
  }

  def hsort(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: hsort(deleteMin(h))
  }

  property("hsort1") = forAll { (h: H) =>
    val ls = hsort(h)
    ls == ls.sorted
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    if (!(isEmpty(h1) || isEmpty(h2))) {
      val min1 = findMin(h1); val min2 = findMin(h2)
      val min = if (min1 < min2) min1 else min2
      min == findMin(meld(h1, h2)) && min == findMin(meld(h2, h1))
    } else true
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
