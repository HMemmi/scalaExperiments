package generics

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
  def main(args: Array[String]) = {
    val h1 = Human("hichem", "hmemmi", 27, 99999999);
    val h2 = Human("ala", "ati", 28, 100000);
    val h3 = Human("hichem", "hmemmi", 27, 99999999);

    h1.eat
    println(h1 > h2);
    
     println(h1 == h3);

    val an1 = Animal("fox", "foc", 32, "big dog", 82)
    val an2 = Animal("rex", "rex", 32, "big dog", 322)

    an1.eat
    println(an1 > an2);

  }
}