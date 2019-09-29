object FP_List extends App {

  sealed trait List[+A]
  case class Cons[+A](x: A, xs: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil =>
        println("nil")
        z
      case Cons(x, xs) =>
        println(s"head $x, tail $xs")
        f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sorted[A](l: List[A], f: (A, A) => Boolean): Boolean = {
    val res = foldLeft(l, (true: Boolean, Nil: List[A])) {
      case ((flag: Boolean, prev: List[A]), next: A) =>
        println(s"flag $flag, prev $prev, next $next")
        if (!flag) {
          (false, Nil)
        }
        else if (prev == Nil) {
          (true, Cons(next,Nil))
        }
        else {
          (f(prev.asInstanceOf[Cons[A]].x,next), Cons(next, Nil))
        }
    }

    res._1
  }

  val zz = foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))
  val zz2 = foldLeft(List(1,2,3), Nil: List[Int])((b,a) => Cons(a, b))

  println (zz)
  println (zz2)

  println(sorted[Int](List(3,2,3), _ < _))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }
//  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
//    case Nil => Nil
//    case Cons(x, xs) =>
//
//  }

  println (map[Int, String](List(1,2,3))((a: Int) => (a+1).toString))
}
