package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.mutable.MutableList

import org.apache.commons.math3.distribution.UniformIntegerDistribution
import org.apache.commons.math3.random.RandomGenerator

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.kernel.View
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Height

class HeightView(val rng: RandomGenerator) extends View {
  def dimension = 2
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]): Double = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val j = configuration.K match {
        case 0 => 0
        case _ => new UniformIntegerDistribution(rng, 0, configuration.K).sample
      }
      val height1 = configuration.height(j)
//      println("\tpdf height for j = " + j + " h = " + height1 + " with k = " + configuration.K)
      output += height1
      output += j
      1. / (configuration.K + 1)
    case _ => 0.
  }
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val iterator = input.iterator
      val height1 = iterator.next
      val j = iterator.next.toInt
//      println("\tinversePdf height for j = " + j + " hh = " + height1 + " with k = " + configuration.K)
      modification.height = Height(j, height1)
      1. / (configuration.K + 1)
    case _ => 0.
  }
}