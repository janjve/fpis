import ExercisesChapter3.{Branch, Leaf, Tree}
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

  "Exercise 3.25" should "get size correctly for trees" in {
    val sut = ExercisesChapter3.size[Int](_:Tree[Int])
    sut(new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Leaf[Int](3))) should be (5)
  }

  "Exercise 3.26" should "get maximum correctly for trees" in {
    val sut = ExercisesChapter3.maximum(_)
    sut(new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Leaf[Int](3))) should be (3)
    sut(new Branch[Int](new Branch[Int](new Leaf[Int](3), new Leaf[Int](2)), new Leaf[Int](1))) should be (3)
    sut(new Branch[Int](new Branch[Int](new Leaf[Int](2), new Leaf[Int](3)), new Leaf[Int](1))) should be (3)
  }

  "Exercise 3.27" should "get depth correctly for trees" in {
    val sut = ExercisesChapter3.depth[Int](_:Tree[Int])
    sut(new Leaf[Int](1)) should be (1)
    sut(new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))) should be (2)
    sut(new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Leaf[Int](3))) should be (3)
  }

  "Exercise 3.28" should "map correctly for trees" in {
    val sut = ExercisesChapter3.map[Int, Int](_:Tree[Int])(x => x+1)
    sut(new Leaf[Int](1)) should be (new Leaf[Int](2))
    sut(new Branch[Int](new Leaf[Int](1), new Leaf[Int](2))) should be (new Branch[Int](new Leaf[Int](2), new Leaf[Int](3)))

    val t = new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Leaf[Int](3))
    sut(t) should be (new Branch[Int](new Branch[Int](new Leaf[Int](2), new Leaf[Int](3)), new Leaf[Int](4)))
    ExercisesChapter3.size(sut(t)) should be (ExercisesChapter3.size(t))
  }

  "Exercise 3.29" should "fold correctly for trees" in {
    val size = ExercisesChapter3.sizeViaFold[Int](_:Tree[Int])
    val max = ExercisesChapter3.maximumViaFold(_)
    val depth = ExercisesChapter3.depthViaFold[Int](_:Tree[Int])
    val map = ExercisesChapter3.mapViaFold[Int, Int](_:Tree[Int])(x => x+1)

    val t = new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Leaf[Int](4))
    size(t) should be (5)
    max(t) should be (4)
    depth(t) should be (3)
    map(t) should be (new Branch[Int](new Branch[Int](new Leaf[Int](2), new Leaf[Int](3)), new Leaf[Int](5)))
  }
}
