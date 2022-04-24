import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.concurrent.duration.TimeUnit

object Parallel {
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }
  def map2ViaFlatMapUnit[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = {
    flatMap(pa)(a => flatMap(pb)(b => unit(f(a,b))))
  }
  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] =
    map2(map2(pa, pb)((a, b) => (c:C) => f(a,b,c)), pc)((cd: C=>D, c) => cd(c))
  def map4[A,B,C,D,E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A,B,C,D) => E): Par[E] =
    map2(map3(pa,pb,pc)((a,b,c) => (d:D) => f(a,b,c,d)), pd)((de: D=>E, d) => de(d))

  def foldRight[A,B](parList: Par[List[A]], parB: Par[B])(f: (A,B) => B): Par[B] =
    map2(parList, parB)((lsA ,b) => lsA.foldRight(b)(f))
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](lazyUnit(List[A]))((a,b) => map2(a,b)(_ :: _))
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(parMap[A, List[A]](as)(a => if (f(a)) List(a) else List()))(_.flatten)
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
  def parFoldRight[A,B](ls: List[A])(b: B)(f: (A,B) => B): Par[B] =  {
    foldRight(lazyUnit(ls), lazyUnit(b))(f)
  }
  def wordCount(paragraphs: List[String]) : Par[Int] = {
    parFoldRight[String, Int](paragraphs)(0)((s,i) => i + s.split(" ").length)
  }
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser[Boolean, A](cond)(b => if(b) t else f)
  }
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser[Int, A](n)(choices)
  }
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es => choices(key(es).get)(es)
  }
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => choices(pa(es).get)(es)
  }
  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = chooser(a)(f)
  def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get()(es)
  def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(pa => pa)
  def flatMapUsingJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
}