import ExercisesChapter4._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by jan on 12/21/2016.
  */
  class ExercisesChapter4Test extends FlatSpec with Matchers {
    "Exercise 4.1" should "map, flatmap, getOrElse, orElse and filter Option correctly" in {
      val o = new ExercisesChapter4.Some(1)
      val no = None
      o.map(x => x+1) should be (Some(2))
      o.flatMap(x => None) should be (None)
      o.getOrElse(3) should be (1)
      no.getOrElse(3) should be (3)
      o.orElse(Some(3)) should be (o)
      no.orElse(Some(3)) should be (Some(3))

      o.filter(x => x > 2) should be (None)
      o.filter(x => x < 2) should be (o)
    }

    "Exercise 4.2" should "calculate variance correctly" in {
      val sut = ExercisesChapter4.variance(_)
      sut(List(1.0, 1.0, 3.0, 3.0)) should be (Some(1))
      sut(List(1.0, 1.0, 4.0, 4.0)) should be (Some(2.25))
      sut(List()) should be (None)
    }

    "Exercise 4.3" should "map 2 options correctly" in {
      val sut = ExercisesChapter4.map2[Int, Int, Int](_: Option[Int], _: Option[Int])((x, y) => x+y)
      sut(Some(1), Some(2)) should be (Some(3))
      sut(Some(1), None) should be (None)
    }

    "Exercise 4.4" should "sequence options correctly" in {
    val sut = ExercisesChapter4.sequence[Int](_: List[Option[Int]])
    sut(List(Some(1), Some(2), Some(3))) should be (Some(List(1,2,3)))
    sut(List(Some(1), Some(2), None)) should be (None)
    }

    "Exercise 4.5" should "sequenceViaTraverse options correctly" in {
      val sut = ExercisesChapter4.sequenceViaTraverse[Int](_: List[Option[Int]])
      sut(List(Some(1), Some(2), Some(3))) should be (Some(List(1,2,3)))
      sut(List(Some(1), Some(2), None)) should be (None)
    }

  "Exercise 4.6" should "map, flatmap, orElse and map2 Either correctly" in {
    val l = new ExercisesChapter4.Left(-1)
    val r = new ExercisesChapter4.Right(1)
    l.map(x => 1) should be (l)
    r.map(x => x+1) should be (Right(2))

    l.flatMap(x => Right(x)) should be (l)
    r.flatMap(x => Right(x+1)) should be (Right(2))

    l.orElse(Right(3)) should be (Right(3))
    r.orElse(Right(3)) should be (r)

    l.map2(l)((ll,rr) => s"$ll,$rr") should be (l)
    l.map2(r)((ll,rr) => s"$ll,$rr") should be (l)
    r.map2(l)((ll,rr) => s"$ll,$rr") should be (l)
    r.map2(r)((ll,rr) => s"$ll,$rr") should be (Right("1,1"))
  }

  "Exercise 4.7" should "sequenceViaTraverse Either correctly" in {
    val sut = ExercisesChapter4.sequence[Int, Int](_: List[Either[Int, Int]])
    sut(List(Right(1), Right(2), Right(3))) should be (Right(List(1,2,3)))
    sut(List(Left(1), Right(2), Left(3))) should be (Left(1))
    sut(List(Right(1), Right(2), Left(3))) should be (Left(3))
  }
}
