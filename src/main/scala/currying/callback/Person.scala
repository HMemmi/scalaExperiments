package currying.callback

class Person private (val name: String, var firstName: String)(val callback: (Option[Int], Person) => Unit) {
  var age = None: Option[Int]
  var partner = None: Option[Person]

  //def this() = this("", "")
  def +(p: Person) = this.partner = Option(p);
  def -() = this.partner = None: Option[Person]
  def myPartner = this.partner

  callback(age, this)
}

object Person {
  var count = 0
  def apply(name: String = "Zaineb", firstName: String = "Madhi")(): Person = new Person(name, firstName)(
    (age: Option[Int], p: Person) => {
      println("created a new Person! ", age, p.name)
      this.count += 1
    })
}

/*case class H(name: String, nickname: String, age: Int, salary: Int) extends Creature{
    val what = "some rice"
  def compare(that: Creature): Int = (this.salary - that.asInstanceOf[H].salary)
}*/

object main {
  def main(args: Array[String]): Unit = {
    val p1 = Person("hichem", "memmi");
    p1.firstName = "aa";
    println("Count " + Person.count)
    val p2 = Person();
    println("Count " + Person.count)
    p2.age = Option(12)

    val s = { x: Int => x * 2 }
    val s1: Int => Int = { _ * 2 }
    val s2 = { (_: Int) * 2 }

    p1 + p2

    println("My partner is " + p1.myPartner.get.name)
    p1 -

    println("My partner is " + p1.myPartner)

  }
}