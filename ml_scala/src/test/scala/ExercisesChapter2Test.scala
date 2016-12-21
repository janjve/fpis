import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jan on 12/21/2016.
  */
class ExercisesChapter2Test extends FlatSpec with Matchers {
  "Exercise 2.1" should "hold for the Fibonachi properties" in {
    val sut = ExercisesChapter2.fib(_)

    sut(0) should be (0)
    sut(1) should be (1)
    sut(2) should be (1)
    sut(3) should be (2)
    sut(4) should be (3)
    sut(5) should be (5)
  }

  "Exercise 2.2" should "hold for ascending ordering" in {
    val sut = (ns: Array[Int]) => ExercisesChapter2.isSorted(ns, (x: Int,y: Int) => x <= y)

    sut(Array()) should be (true)
    sut(Array(1)) should be (true)
    sut(Array(1,2)) should be (true)
    sut(Array(1,2,3)) should be (true)
    sut(Array(1,3,2)) should be (false)
    sut(Array(2,1,3)) should be (false)
    sut(Array(3,2,1)) should be (false)
    sut(Array(2,1)) should be (false)
  }

  "Exercise 2.2" should "hold for descending ordering" in {
    val sut = (ns: Array[Int]) => ExercisesChapter2.isSorted(ns, (x: Int,y: Int) => x >= y)

    sut(Array()) should be (true)
    sut(Array(1)) should be (true)
    sut(Array(1,2)) should be (false)
    sut(Array(1,2,3)) should be (false)
    sut(Array(1,3,2)) should be (false)
    sut(Array(2,1,3)) should be (false)
    sut(Array(3,2,1)) should be (true)
    sut(Array(2,1)) should be (true)
  }
}
