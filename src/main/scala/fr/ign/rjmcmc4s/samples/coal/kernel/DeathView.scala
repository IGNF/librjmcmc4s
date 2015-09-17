package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.mutable.MutableList
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.kernel.View
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Death
import org.apache.commons.math3.distribution.UniformIntegerDistribution
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Birth

class DeathView(val rng: RandomGenerator) extends View {
  def dimension = 5
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]): Double = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      if (configuration.K < 1) return 0.
      val j = configuration.K match {
        case 1 => 0
        case _ => new UniformIntegerDistribution(rng, 0, configuration.K - 1).sample
      }
      val height0 = configuration.height(j)
      val height1 = configuration.height(j + 1)
      val position0 = configuration.position(j)
      val position1 = configuration.position(j + 1)
      val position2 = configuration.position(j + 2)
//      println("\tdeath pdf j = " + j + " s1 = " + position1 + " with K = " + configuration.K)
      output += height0
      output += height1
      output += position0
      output += position1
      output += position2
      1. / configuration.K
  }
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val iterator = input.iterator
      val height0 = iterator.next
      val height1 = iterator.next
      val position0 = iterator.next
      val ss = iterator.next
      val j = configuration.S.lastIndexWhere(x => ss > x)
//      println("\tdeath inversePdf  ss = "  + ss + " j = " + j + " h0 = " + height0 + " h1 = " + height1 + " p0 " + position0 + " with K = " + configuration.K)
      modification.birth = Birth(j, height0, height1, ss)
      1. / (configuration.K + 1)
  }
}