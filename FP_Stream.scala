object FP_Stream extends App {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = uncons.map { case (a, st) => a :: st.toList } getOrElse Nil

    //    def take(n: Int): Stream[A] =
    //      if (n < 1) Stream.empty
    //      else uncons.map { case (a, st) => Stream.cons(a, st.take(n-1)) } getOrElse Stream.empty

    //    def takeWhile(p: A => Boolean): Stream[A] =
    //      uncons.map { case (a, st) =>
    //        if (p(a)) Stream.cons(a, st.takeWhile(p))
    //        else Stream.empty
    //      } getOrElse Stream.empty

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        //allows early termination if 'f' chooses not to evaluate lazy B param
        //this trick used in 'exists' and 'forAll'
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    //    def takeWhile(p: A => Boolean): Stream[A] =
    //      foldRight(Stream.empty: Stream[A])((a,b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

    //    def map[B](f: A => B): Stream[B] =
    //      foldRight(Stream.empty: Stream[B])((a,b) => Stream.cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    def append[B >: A](h: B): Stream[B] =
      foldRight(Stream(h))((a, b) => Stream.cons(a, b))

    def appendAll[B >: A](st: Stream[B]): Stream[B] =
      foldRight(st)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty: Stream[B])((a, b) => f(a).appendAll(b))

    //unfold -> map take takeWhile zip , (as in chapter 3), and zipAll
    def map[B](f: A => B): Stream[B] =
      Stream.unfold(uncons) {
        case Some((a, st)) => Some((f(a), st.uncons))
        case None => None
      }

    def take(n: Int): Stream[A] =
      Stream.unfold((n, uncons)) {
        case (n, Some((a, st))) if n > 0 =>
          Some((a, (n - 1, st.uncons)))
        case _ => None
      }

    def takeWhile(p: A => Boolean): Stream[A] =
      Stream.unfold(uncons) {
        case Some((a, st)) if p(a) => Some((a, st.uncons))
        case _ => None
      }

    def zip[B](that: Stream[B]): Stream[(A, B)] =
      Stream.unfold((uncons, that.uncons)) {
        case (Some((a, stA)), Some((b, stB))) =>
          val nextVal = (a, b)
          val nextState = (stA.uncons, stB.uncons)
          Some(nextVal, nextState)
        case _ => None
      }

    def zipAll[A1 >: A, B](that: Stream[B], thisEl: A1, thatEl: B): Stream[(A1, B)] =
      Stream.unfold((uncons, that.uncons)) {
        case (Some((a, stA)), Some((b, stB))) =>
          val nextVal = (a, b)
          val nextState = (stA.uncons, stB.uncons)
          Some(nextVal, nextState)
        case (None, Some((b, stB))) =>
          val nextVal = (thisEl, b)
          val nextState = (None, stB.uncons)
          Some(nextVal, nextState)
        case (Some((a, stA)), None) =>
          val nextVal = (a, thatEl)
          val nextState = (stA.uncons, None)
          Some(nextVal, nextState)
        case _ => None
      }

    def tails: Stream[Stream[A]] =
      scanRight(Stream.empty: Stream[A])(Stream.cons(_,_))
//      Stream
//        .unfold(
//          Some(this): Option[Stream[A]]
//        )(_.map(
//          st => (st, st.uncons.map(_._2))
//        ))

    def startsWith[A1 >: A](that: Stream[A1]): Boolean = {
      val (l, r) = (uncons, that.uncons)

      val comparisonResult = for {
        ll <- l
        rr <- r
      } yield (ll._1 == rr._1, (ll._2, rr._2))

      l.isDefined && comparisonResult.forall {
        case (true, (sa, sb)) => sa.startsWith(sb)
        case _ => false
      }

    }

    def hasSubsequence[A1 >: A](that: Stream[A1]): Boolean =
      tails.exists(_.startsWith(that))

    //generalized 'tails'
    //Stream(1,2,3).scanRight(0)(_ + _).toList ==> List(6, 5, 3, 0)
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
      Stream
        .unfold(
          Some(this): Option[Stream[A]]
        )(_.map(
          st => (st.foldRight(z)(f), st.uncons.map(_._2))
        ))

  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    //    def ones: Stream[Int] = cons(1, ones)
    //
    //    def constant[A](a: A): Stream[A] = cons(a, constant(a))
    //
    //    def from(n: Int): Stream[Int] = cons(n, from(n+1))
    //
    //    def fibs: Stream[Int] = {
    //      def go(prev: Int, curr: Int): Stream[Int] = {
    //        val next = prev + curr
    //        cons(next, go(curr, next))
    //      }
    //
    //      Stream(0, 1).appendAll(go(0, 1))
    //    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Stream.empty
    }

    def ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

    def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def fibs: Stream[Int] =
      Stream(0, 1).appendAll(unfold((0, 1): (Int, Int)) { case (prev, curr) =>
        val next = prev + curr
        val tup = (next, (curr, next))
        Some(tup)
      })

  }

  println(Stream(1, 2, 3))
  println(Stream(1, 2, 3).toList)
  println("take ===> " + Stream(1, 2, 3, 4).take(2).toList)
  println("take while ===> " + Stream(1, 2, 3, 4).takeWhile(_ < 4).toList)

  println(Stream(1, 2, 3, 4).exists(_ == 3))
  println(Stream(1, 2, 3, 4).exists(_ > 4))
  println(Stream(1, 2, 3).forAll(_ > 0))
  println(Stream(1, 2, 3).forAll(_ < 3))


  println("map ===> " + Stream(1, 2, 3).map(_ + 10).toList)
  println(Stream(1, 2, 3, 4, 5).filter(_ < 4).toList)

  println(Stream(1, 2).append(7).toList)
  println(Stream(1, 2).appendAll(Stream(5, 6)).toList)

  println(Stream(1, 2).flatMap(a => Stream(a + 10, a + 20)).toList)

  println(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)

  println(Stream.ones.take(5).toList)
  println(Stream.constant(5).take(3).toList)
  println(Stream.from(5).take(3).toList)
  println(Stream.fibs.take(10).toList)

  println(Stream.unfold(1)(s => if (s < 5) Some((s, s + 1)) else None).toList)

  println(Stream(1, 2, 3, 4, 5).zip(Stream(7, 6, 5)).toList)
  println(Stream(1, 2, 3, 4, 5).zipAll(Stream(7, 6, 5), -1, -2).toList)

  println(Stream(1, 2, 3).tails.toList.map(_.toList))

  println(Stream().startsWith(Stream(3,4,5)))

  val s10 = Stream.from(1).take(10)
  val s9_4 = Stream.from(6).take(4)
  println(s10.toList)
  println(s9_4.toList)
  println(s10.tails.toList.map(_.toList))
  println(s10.hasSubsequence(s9_4))

  println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  println(Stream(1,2,3).scanRight(Stream.empty: Stream[Int])(Stream.cons(_,_)).toList.map(_.toList))
  println(Stream(1, 2, 3).tails.toList.map(_.toList))

}
