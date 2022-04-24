object Monoid {
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    val zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b
    val zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a || b
    val zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b
    val zero: Boolean = true
  }
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o1.orElse(o2)
    val zero: Option[A] = None
  }
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): (A => A) = f1 compose f2
    val zero: A => A = (a: A) => a
  }
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }
  def foldLeftUsingFoldMap[A](as: List[A], z: A)(op: (A, A) => A): A = {
    foldMap(as, new Monoid[A] {
      override def op(a: A, b: A): A = op(a,b)
      val zero = z
    })(a => a)
  }
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.length match {
      case 0 => m.zero
      case 1 => f(v.head)
      case _ =>
        val (v1, v2) = v.splitAt(v.length/2)
        m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
    }
  }
  def isOrdered[Int](as: List[Int]): Boolean = {
    false
  }
  //10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(w1: WC, w2: WC): WC  = {
      (w1, w2) match {
        case (Part(l1, c1, r1), Part(l2, c2, r2)) =>
          if (r1.endsWith(" ") && l2.startsWith(" ")) Part(l1,c1+c2,r2)
          else Part(l1, c1+c2+1, r2)
        case (part @ Part(l, c, r), Stub(s)) =>
          if (s.contentEquals(" ") && r.endsWith(" ")) part
          else if (s.startsWith(" ") && !r.endsWith(" ")) Part(l,c+1,s)
          else if (s.endsWith(" ")) Part(l, c+1, " ")
          else Part(l, c, r+s)
        case (Stub(s), part @ Part(l, c, r)) =>
          if (s.contentEquals(" ") && r.startsWith(" ")) part
          else if (s.startsWith(" ")) Part(" ",c+1,r)
          else if (s.endsWith(" ") && !l.startsWith(" ")) Part(s, c+1, r)
          else Part(s+l, c, r)
        case (Stub(s1),Stub(s2)) =>
          if (s1.startsWith(" ") && s2.startsWith(" ")) Part(" ", 1, s2.tail)
          else if (s1.startsWith(" ") && s2.endsWith(" ")) Part(" ", 1, " ")
          else if (s1.endsWith(" ") && s2.endsWith(" ")) Part(s1, 1, " ")
          else Stub(s1+s2)
      }
    }
    val zero = Stub("")
  }
  class FoldableList extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
      as.foldRight(mb.zero)((a: A,b: B) => mb.op(f(a), b))
    }
    def toList[A](as: List[A]): List[A] = as
  }
  class FoldableIndexedSeq extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = {
      as.length match {
        case 0 => mb.zero
        case 1 => f(as.head)
        case _ =>
          val (as1, as2) = as.splitAt(as.length/2)
          mb.op(foldMap(as1)(f)(mb), foldMap(as2)(f)(mb))
      }
    }
    def toList[A](as: IndexedSeq[A]) = as.toList
  }
  val opFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case Some(a) => f(a,z)
      case None => z
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      foldRight(as)(z)((b,a) => f(a,b))

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))
    }
  }
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      val zero = (a.zero,b.zero)

      override def op(t1: (A, B), t2: (A, B)): (A, B) = (a.op(t1._1, t2._1), b.op(t1._2, t2._2))
    }
  }
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      val zero: A => B = (_:A) => B.zero

      override def op(a: A => B, b: A => B): A => B = {
        x: A => B.op(a(x), b(x))
      }
    }
  }
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = new Monoid[Map[A,Int]] {
      override def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] = {
        (a.keySet ++ b.keySet).foldLeft(zero)((acc, k) => {
          acc.updated(k, a.getOrElse(k,0) + b.getOrElse(k,0))
        })
      }
      override val zero: Map[A, Int] = Map.empty
    }
    as.length match {
      case 0 => m.zero
      case _ => foldMapV(as, m)(a => Map(a->1))
    }
  }
}
trait Monoid[A] {
  def op(a: A, b: A): A
  val zero: A
}
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}