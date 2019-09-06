package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- frequency((1, const(empty)), (9, genHeap))
  } yield insert(i, h) 
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  property("Main check") = forAll {
    (h1: H, h2: H) =>
      def isEqHeap(h1: H, h2: H): Boolean = {
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          val m1 = findMin(h1)
          val m2 = findMin(h2)
          m1 == m2 && isEqHeap(deleteMin(h1), deleteMin(h2))
        } 
      }
      isEqHeap(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
