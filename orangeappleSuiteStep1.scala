package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class orangeappleSuite extends FunSuite {

  import common._
    

  trait TestLists {
  val purchases1: List[String] = List("apple", "apple", "orange", "apple")
  val purchases2: List[String] = List()
  val purchases3: List[String] = List("apple", "banana", "orange", "apple")
  }


  test("totals are correct") {
    new TestLists {
      val s1 = orangeapple.totalPuchases(purchases1)
      assert(s1 === 2.05)
      val s2 = orangeapple.totalPuchases(purchases2)
      assert(s2 === 0.0)
      val s3 = orangeapple.totalPuchases(purchases3)
      assert(s3 === 1.45)
    }
  }

}