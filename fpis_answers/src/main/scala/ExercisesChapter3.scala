/**
  * Created by jan on 12/21/2016.
  */
object ExercisesChapter3 {
  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case h :: t => t
    case Nil => Nil
  }

  // 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case h :: t => a :: t
    case Nil => Nil
  }

  // 3.4
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case h :: t => if(n > 0) drop(t, n-1) else t
    case Nil => Nil
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case h :: t => if(f(h)) l else dropWhile(t, f)
    case Nil => Nil
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l.reverse match {
    case h :: t => t.reverse
    case Nil => Nil
  }

  // 3.9
  def length[A](as: List[A]): Int = as.foldRight(0)((a,b) =>b+1)

  // 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case h :: t => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  // 3.11
  def sum(ns: List[Int]) = foldLeft(ns, 0)((b,a) => b+a)
  def product(ns: List[Double]) = foldLeft(ns, 1.0)((b,a) => b*a)

  // 3.12
  def reverse[A](ns: List[A]) = foldLeft(ns, List.empty[A])((b,a) => a :: b)

  // 3.13
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((g,a) => b => g(f(a,b)))(z)

  // 3.14
  def append[A](as: List[A], a: A): List[A] =foldRight(as, List(a))((a,b) => a :: b)

  // 3.15
  def flatten[A](as: List[List[A]]): List[A] = foldRight(as, List.empty[A])((a, b) => foldRight(a, b)((a1, b1) => a1 :: b1))

  // 3.16
  def addOne(as: List[Int]): List[Int] = foldRight(as, List.empty[Int])((a,b) => (a+1) :: b)

  // 3.17
  def convertToString(as: List[Double]): List[String] = foldRight(as, List.empty[String])((a,b) => a.toString :: b)

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, List.empty[B])((a,b) => f(a) :: b)

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List.empty[A])((a,b) => if(f(a)) a :: b else b)
  def evens(as: List[Int]): List[Int] = filter(as)(x => x%2 == 0)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List.empty[B])((a,b) => f(a) ::: b)

  // 3.21
  def filterViaFlapmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap[A,A](as)((a: A) => if(f(a)) List(a) else List.empty)

  // 3.22
  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (ah :: at, bh :: bt) => ah + bh :: addPairwise(at, bt)
  }

  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (ah :: at, bh :: bt) => f(ah,bh) :: zipWith(at, bt)(f)
  }


  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1+size(l)+size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  // 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => new Leaf[B](f(v))
    case Branch(l, r) => new Branch[B](map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold[A,Int](t)(b => 1)((l,r) => l+r+1)
  def maximumViaFold(t: Tree[Int]): Int = fold[Int,Int](t)(b => b)((l,r) => l.max(r))
  def depthViaFold[A](t: Tree[A]): Int = fold[A,Int](t)(b => 1)((l,r) => l.max(r) + 1)
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A,Tree[B]](t)(a => new Leaf(f(a)))((l,r) => new Branch(l, r))
}