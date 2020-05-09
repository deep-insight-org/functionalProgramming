package core

import cats.{Monoid, Show}
import cats.implicits._
import cats.kernel.Semigroup

object First extends App {
  //having state and behaviour
  case class Adder(int1: Int, int2: Int) {
    def add: Int = int1 + int2
  }
  case class Formatter(int: Int) {
    def format: String = s"This is the beauty of formatting $int"
  }

  println("_" * 100)
  println(new Adder(12, 13).add)
  println(new Formatter(new Adder(12, 13).add).format)
  println("_" * 100)

  //state and behaviour separate

  case class Data[T](t: T)

  def add[A:Semigroup](a1:A, a2:A):A = implicitly[Semigroup[A]].combine(a1,a2)

  implicit val behaviourOfAddForData = new Semigroup[Data[Int]] {
    override def combine(x: Data[Int], y: Data[Int]): Data[Int] = Data(x.t + y.t)
  }

  implicit val formaterData = new Show[Data[Int]] {
    override def show(t: Data[Int]): String = s"This is the beauty of formatting by show $t"
  }

  println("_" * 100)
  println(add(Data(12),Data(14)).show)
  println("_" * 100)

  //sum type OR
  sealed trait Mood
  case object Boring extends Mood
  case object Fun extends Mood

  sealed trait Temp
  case object High extends Temp
  case object Low extends Temp
  case object Mid extends Temp


  def changeMood(mood: Mood):Mood = mood match {
    case Boring => Fun
    case _ => Boring
  }

  println(changeMood(Boring))

  //product Type AND 2 * 3 = 6
  case class TempMood(mood: Mood, temp: Temp)

  List(1,2,3,4)
}
