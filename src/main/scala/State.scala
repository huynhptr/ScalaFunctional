case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a,ss) = run(s)
      f(a).run(ss)
    })
  def map[B](f: A=>B): State[S, B] = flatMap(a => State(s => (f(a), s)))
  def map2[B, C](sb: State[S, B])(f: (A,B) => C): State[S,C] = flatMap(a => State(s => {
    val (b, ss) = sb.run(s)
    (f(a,b),ss)
  }))
}

object State {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[S, A](a: A): S => (A,S) = (s: S) => (a, s)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }
  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] = {
    def helper(rs: List[State[S,A]], soFar: List[A]): State[S, List[A]] =
      State(s => rs match {
        case Nil => (soFar, s)
        case h :: t =>
          val (a, ss) = h.run(s)
          helper(t, soFar ++ List(a)).run(ss)
      })
    helper(fs, List.empty)
  }
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(rng => nonNegativeInt(rng))(i => if (i + (n-1) - i%n >= 0) unit(i%n) else nonNegativeLessThan(n))
  }
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
  val int: Rand[Int] = _.nextInt
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, g) = rng.nextInt
    (if(i < 0) -(i+1) else i, g)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, g) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, g)
  }
  def double2(rng: RNG): (Double, RNG) = {
    map(int)(_.toDouble)(rng)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, g1) = int(rng)
    val (d, g2) = double(g1)
    ((i,d), g2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (idTup, g) = intDouble(rng)
    (idTup.swap, g)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def helper(count: Int, lastRng: RNG, soFar: List[Int]): (List[Int], RNG) = {
      count match {
        case 0 => (soFar, lastRng)
        case _ =>
          val (i, g) = lastRng.nextInt
          helper(count-1, g, i::soFar)
      }
    }
    helper(count, rng, List.empty)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(State(int))).run(rng)
  }
}
