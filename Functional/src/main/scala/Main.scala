import Candy.simulateMachine
import OptionFunctional._
import Monoid._
import State._

object Main {
  def main(args: Array[String]): Unit = {
    val listOp: List[Option[Int]] = List(Some(1),None, Some(2))
    val listOp2: List[Option[Int]] = List(Some(1), Some(2))
    println(OptionFunctional.sequence(listOp2))
    println(from(1).takeWhile(_ == 1))
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n))

    val simpleRNG = SimpleRNG(1)
    println("Ints: " + ints(5)(simpleRNG)._1)
    println("Nonnegative: " + nonNegativeInt(simpleRNG)._1)
    println("Double: " + double(simpleRNG)._1)
    println("Nonnegative less than 1000: " + nonNegativeLessThan(1000)(simpleRNG)._1)

    val machine = Machine(true, 5, 10)
    val v = simulateMachine(List(Coin, Turn,Coin, Turn,Coin, Turn,Coin, Turn)).run
  }
}