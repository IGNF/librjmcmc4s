package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.mutable.MutableList
import org.apache.commons.math3.distribution.UniformIntegerDistribution
import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.kernel.View
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Birth
import org.apache.commons.math3.distribution.UniformRealDistribution
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Death

class BirthView(implicit val rng: RandomGenerator) extends View {
  def dimension = 4
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]): Double = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val ss = new UniformRealDistribution(rng, 0, configuration.L).sample//34059.98000877121
      val j = configuration.S.lastIndexWhere(x => ss > x)
      val height0 = configuration.height(j)
      val position0 = configuration.position(j)
      val position1 = configuration.position(j + 1)
//      println("\tbirth pdf ss = "  + ss + " j = " + j + " h0 = " + height0 + " p0 " + position0 + " p1 = " + position1 + " with K = " + configuration.K)
      output += height0
      output += position0
      output += position1
      output += ss
      1.0 / configuration.L
  }
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val iterator = input.iterator
//      val height0 = iterator.next
      val height1 = iterator.next
      val position0 = iterator.next
      val position2 = iterator.next
      val position1 = iterator.next
      val j = configuration.S.lastIndexWhere(x => position1 > x)
//      println("\tbirth inversePdf j = " + j + " with K = " + configuration.K)
      modification.death = Death(j, height1)
      1.0 / configuration.L
  }
}