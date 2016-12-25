/**
  * Created by jan on 12/21/2016.
  */
object ExercisesChapter4 {


  trait Option[+A] {
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] =
      this.flatMap(x => if(f(x)) Some(x) else None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.size > 0) Some(xs.sum / xs.size) else None

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

}