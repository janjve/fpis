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

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(av), Some(bv)) => Some(f(av, bv))
      case _ => None
    }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: t => h.flatMap(a => sequence(t).map(a :: _))
      case Nil => Some(Nil)
    }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h), t)(_::_))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse[Option[A], A](a)(x => x)


  // 4.6
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(b) => Left(b)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(b) => Left(b)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case _ => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap { case a => b.map { case bb => f(a, bb) } }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  // 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h,t) => f(h).map2(t)(_::_))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse[E, Either[E, A], A](es)(x => x)
}