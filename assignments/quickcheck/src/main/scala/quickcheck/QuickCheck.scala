package quickcheck

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.{Arbitrary, Gen, Properties}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- Arbitrary.arbitrary[Int]
    heap <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(i, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def heapToList(h: H): List[A] = {
    if (isEmpty(h)) List[A]()
    else {
      findMin(h) :: heapToList(deleteMin(h))
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min elements in empty heap") = forAll {
    (a: A, b: A) => {
      findMin(insert(a, insert(b, empty))) == (if (a < b) a else b)
    }
  }

  property("insert in empty heap") = forAll {
    (a: A) => {
      isEmpty(deleteMin(insert(a, empty)))
    }
  }

  property("sorting elements with deleteMin") = forAll {
    (h: H) => {
      val sortedHeap = heapToList(h)
      sortedHeap == sortedHeap.sorted
    }
  }

  property("min of two melded heaps") = forAll {
    (h1: H, h2: H) => {
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val minMelded = findMin(meld(h1, h2))
      minMelded == minH1 || minMelded == minH2
    }
  }

  property("meld mixin min value") = forAll {
    (h1: H, h2: H) => {
      val melded = meld(h1, h2)
      val newMelded = meld(insert(findMin(h1), h2), deleteMin(h1))
      heapToList(melded) == heapToList(newMelded)
    }
  }
}
