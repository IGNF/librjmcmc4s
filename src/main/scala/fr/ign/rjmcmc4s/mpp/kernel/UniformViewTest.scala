package fr.ign.rjmcmc4s.mpp.kernel

import scala.collection.mutable.MutableList
import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.mpp.configuration.ListBufferConfiguration
import org.apache.commons.math3.random.MersenneTwister

object UniformViewTest extends App {
  class TestObject(rng: RandomGenerator) extends Iterable[Double] {
    class TestObjectIterator extends Iterator[Double] {
      var index = -1
      def hasNext = index < 1
      def next = {
        index += 1
        index match {
          case 0 => x
          case 1 => y
        }
      }
    }
    //    var container: MutableList[Double] = new MutableList[Double]()
    var x: Double = rng.nextDouble()
    var y: Double = rng.nextDouble()
    def iterator: Iterator[Double] = new TestObjectIterator
    override def toString(): String = "(" + x + ", " + y + ")"
  }
  val rng = new MersenneTwister(1111111)
  class TestObjectBuilder extends ObjectBuilder[TestObject] {
    def dimension = 2
    def create(it: Iterable[Double]) =
      {
        val t = new TestObject(rng)
        val iterator = it.iterator
        t.x = iterator.next
        t.y = iterator.next
        t
      }
  }

  var c = new ListBufferConfiguration[TestObject](0, 0)
  c.insert(new TestObject(rng))
  c.insert(new TestObject(rng))
  c foreach (x => {
    println("new object = " + x)

  })
  val m = c.modification
  println("size = " + c.size)
  val view = new UniformView[TestObject](rng, 1, new TestObjectBuilder)
  val output = new MutableList[Double]
  val p = view.pdf(c, m, output)
  println("p = " + p)
  output foreach (x => print(x + " "))
  println
  println("modif = " + m)
  val p1 = view.inversePdf(c, m, output)
  println("p1 = " + p1)
  println("modif = " + m)
}
