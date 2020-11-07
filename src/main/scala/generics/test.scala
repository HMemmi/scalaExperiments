package generics

import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

sealed abstract class Creature[A <: Creature[A]] extends Eat with Ordered[A] {
  def name: String
  def nickname: String
  def age: Int
  //def compare(that: A): Int=(this.age-that.age)
}

trait Eat {
  def what: String
  def eat() = {
    println(what);
  }
}

case class Human(name: String, nickname: String, age: Int, salary: Int) extends Creature[Human] {
  val what = "some rice"
  def compare(that: Human): Int = (this.salary - that.age)
}

case class Animal(name: String, nickname: String, age: Int, typeof: String, weight: Int) extends Creature[Animal] {
  val what = "love to eat Human!"
  override def compare(that: Animal): Int = (this.weight - that.weight)
}

object main {
  def findMeForEach(list: List[Human]): Unit = {
    list.foreach {
      case Human("hichem", _, _, _) => println("it's me")
      case _                        => println("get out")
    }
  }

  def findMe(list: List[Human]): Unit = {
    for (l <- list) {
      if (l.name eq "hichem") println("it's me") else println("get out")
    }
  }

  def findMeRec(list: List[Human]): Unit = {
    list match {
      case Nil => Nil
      case head :: tail => head match {
        case Human("hichem", _, _, _) => {
          findMeRec(tail)
          println("it's me")
        }
        case _ =>
          findMeRec(tail)
          println("drop out")
      }
    }

  }

  def main(args: Array[String]) = {
    val h1 = Human("hichem", "hmemmi", 27, 99999999);
    val h2 = Human("ala", "ati", 28, 100000);
    val h3 = Human("hichem", "hmemmi", 27, 99999999);

    val list: List[Human] = List(h1, h2, h3)
    println("iterative")
    val t4 = System.nanoTime()
    findMe(list)
    val t5 = System.nanoTime()

    println("Elapsed time iterative: " + (t5 - t4) + "ns")

    println("foreach")
    val t0 = System.nanoTime()
    findMeForEach(list)
    val t1 = System.nanoTime()

    println("Elapsed time foreach: " + (t1 - t0) + "ns")
    println("recursive")
    val t2 = System.nanoTime()
    findMeRec(list)
    val t3 = System.nanoTime()

    println("Elapsed time recursive: " + (t3 - t2) + "ns")

    h1.eat
    println(h1 > h2);

    println(h1 == h3);

    val an1 = Animal("fox", "foc", 32, "big dog", 82)
    val an2 = Animal("rex", "rex", 32, "big dog", 322)

    an1.eat
    println(an1 > an2);

  }
}
