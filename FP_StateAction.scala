object FP_StateAction extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    def fixed(int: Int): RNG = new RNG {
      def nextInt: (Int, RNG) = (int, fixed(int))
    }

    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    //    def positiveInt(rng: RNG): (Int, RNG) = {
    //      val f = (r: RNG) => r.nextInt
    //      var res = f(rng)
    //      while (res._1 == Int.MinValue) res = f(res._2)
    //      (Math.abs(res._1), res._2)
    //    }

    //    def double(rng: RNG): (Double, RNG) = {
    //      val (i, r) = positiveInt(rng)
    //      (i.toDouble / Int.MaxValue, r)
    //    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i1, r1) = positiveInt(rng)
      val (i2, r2) = double(r1)
      ((i1, i2), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (i1, r1) = double(rng)
      val (i2, r2) = positiveInt(r1)
      ((i1, i2), r2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (i1, r1) = double(rng)
      val (i2, r2) = double(r1)
      val (i3, r3) = double(r2)
      ((i1, i2, i3), r3)
    }

    //    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    //      if (count < 1)
    //        (Nil, rng)
    //      else {
    //        val (i, r1) = positiveInt(rng)
    //        val (is, r2) = ints(count - 1)(r1)
    //        (i :: is, r2)
    //      }
    //    }

    type Rand[+A] = RNG => (A, RNG)
    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    //    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    //      rng => {
    //        val (a, rng2) = s(rng)
    //        (f(a), rng2)
    //      }

    //    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    //      rng => {
    //        val (a, r1) = ra(rng)
    //        val (b, r2) = rb(r1)
    //        (f(a, b), r2)
    //      }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    //      rng => {
    //        val it = fs.iterator
    //        var r = rng
    //        (List.fill[Int](fs.length)(0).map(_ => {
    //          val (i, rr) = it.next()(r)
    //          r = rr
    //          i
    //        }), r)
    //      }

    //      rng => {
    //        fs.foldRight((Nil: List[A], rng)) { case (rand, (res, rng)) =>
    //          val (i, r) = rand(rng)
    //          (i :: res, r)
    //        }
    //      }
      fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)((c, d) => c :: d))


    def positiveMax(n: Int): Rand[Int] =
      map(positiveInt)(_ % n)

    def double: Rand[Double] =
      map(positiveInt)(_.toDouble / Int.MaxValue)

    def ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(0).map(_ => positiveInt))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r) = f(rng)
        g(a)(r)
      }

    def positiveInt: Rand[Int] = flatMap(int)(a => rng => {
      var res = (a, rng)
      while (res._1 == Int.MaxValue) {
        res = int(res._2)
      }
      (Math.abs(res._1), res._2)
    })

    //    def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))

  }

  println(RNG.ints(5)(RNG.simple(123L)))

  println(RNG.positiveMax(7)(RNG.simple(123L)))
  println(RNG.double(RNG.simple(12345L)))

  val seq = RNG.sequence(
    List(RNG.positiveMax(5), RNG.double, RNG.double, RNG.double)
  )(RNG.simple(123L))
  println(seq)


  //unit map map2 flatMap sequence
  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S,B] =
      State(s => {
        val (a, ss) = run(s)
        f(a).run(ss)
      })

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S,B])(f: (A, B) => C): State[S, C] =
      for {
        a <- this
        b <- sb
      } yield f(a,b)
//      flatMap(a => sb.map(b => f(a, b)))

    def sequence[A1 >: A](fs: List[State[S, A1]]): State[S, List[A1]] =
      (this::fs).foldRight(State.unit[S, List[A1]](List[A1]()))((a, b) => a.map2(b)((c, d) => c :: d))

  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(State.unit[S, List[A]](List[A]()))((a, b) => a.map2(b)((c, d) => c :: d))
    def get[S]: State[S, S] = State(s => (s,s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  val s1 = State(RNG.positiveMax(5))
  val s2 = State(RNG.double)
  val srng = RNG.simple(123L)
  println(s1.map2(s2)((_,_)).run(srng))

  val statefullComputationsCombined = for {
    a <- State.get[Int]
    b <- State.set(10)
    c <- State.get[Int]
    d <- State.set(100)
    e <- State.get[Int]
    _ <- State.modify[Int](_ => 999)
  } yield (a, c, e)

  println(statefullComputationsCombined.run(1))
  println(State.modify[Int](_ => 888).run(2))

}
