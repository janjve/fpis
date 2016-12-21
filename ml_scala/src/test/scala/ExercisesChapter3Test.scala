import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jan on 12/21/2016.
  */
class ExercisesChapter3Test extends FlatSpec with Matchers {
  "Exercise 3.10 to 3.12" should "work with foldLeft implementation" in {
    val sut = ExercisesChapter3.sum(_)
    val sut2 = ExercisesChapter3.product(_)
    sut(List(1,2,3,4)) should be (1+2+3+4)
    sut2(List(1,2,3,4)) should be (1*2*3*4)
  }

  "Exercise 3.13" should "work as foldLeft for associative functions" in {
    val sum = (as: List[Int]) => ExercisesChapter3.foldRight[Int, Int](as, 0)((x, y) => x + y)
    val product = (as: List[Int]) => ExercisesChapter3.foldRight[Int, Int](as, 1)((x, y) => x * y)

    sum(List(1,2,3,4)) should be (1+2+3+4)
    product(List(1,2,3,4)) should be (1*2*3*4)
  }

  "Exercise 3.14" should "append to list" in {
    val sut = ExercisesChapter3.append[Int](_,_)

    sut(List(), 1) should be (List(1))
    sut(List(1,2), 3) should be (List(1,2,3))
  }

  "Exercise 3.15" should "preserve ordering on list" in {
    val sut = ExercisesChapter3.flatten[Int](_)
    sut(List(List(1,2), List(3,4))) should be (List(1,2,3,4))
  }

  "Exercise 3.16" should "add one to every element in list" in {
    val sut = ExercisesChapter3.addOne(_)
    sut(List(1,2,3,4)) should be (List(2,3,4,5))
  }

  "Exercise 3.17" should "add convert list to strings" in {
    val sut = ExercisesChapter3.convertToString(_)
    sut(List(1.0,2.0,3.0,4.0)) should be (List("1.0","2.0","3.0","4.0"))
  }

  "Exercise 3.18" should "map list correctly" in {
    val addTwo = (as: List[Int]) => ExercisesChapter3.map[Int, Int](as)(x => x+2)
    addTwo(List(1,2,3,4)) should be (List(3,4,5,6))
  }

  "Exercise 3.19" should "filter correctly" in {
    val sut = ExercisesChapter3.evens(_)
    sut(List(1,2,3,4)) should be (List(2,4))
  }

  "Exercise 3.20" should "flapmap correctly" in {
    val sut = ExercisesChapter3.flatMap(_:List[Int])(x => List[Int](x, x))
    sut(List(1,2,3,4)) should be (List(1,1,2,2,3,3,4,4))
  }

  "Exercise 3.21" should "filter via flatmap correctly" in {
    val sut = ExercisesChapter3.filterViaFlapmap(_:List[Int])(x => x%2 == 0)
    sut(List(1,2,3,4)) should be (List(2,4))
  }

  "Exercise 3.22" should "add pairwise correctly" in {
    val sut = ExercisesChapter3.addPairwise(_:List[Int], _:List[Int])
    sut(List(1,2), List(3,4)) should be (List(4,6))
  }

  "Exercise 3.23" should "add pairwise using zipWith correctly" in {
    val sut = ExercisesChapter3.zipWith[Int,Int,Int](_:List[Int], _:List[Int])((a:Int,b:Int) => a+b)
    sut(List(1,2), List(3,4)) should be (List(4,6))
  }
}
