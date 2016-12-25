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
}
