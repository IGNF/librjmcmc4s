package fr.ign.rjmcmc4s.samples.coal.kernel

import fr.ign.rjmcmc4s.configuration.Modification
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.distribution.UniformIntegerDistribution
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import scala.collection.mutable.MutableList
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.kernel.View
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Position

class PositionView(implicit val rng: RandomGenerator) extends View {
  def dimension = 3
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]): Double = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      if (configuration.K == 0) return 0.0
      val j = configuration.K match {
        case 1 => 1
        case _ => new UniformIntegerDistribution(rng, 0, configuration.K).sample
      }
//      println("\tpdf position for j = " + j + " with k = " + configuration.K)
      val position = configuration.position(j)

      val position_prev = configuration.position(j - 1)
      val position_next = configuration.position(j + 1)

      output += position_prev
      output += position
      output += position_next
      1.0 / configuration.K
  }
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) =>
      val iterator = input.iterator
      val position_prev = iterator.next
      val position = iterator.next
      val position_next = iterator.next
      val j = configuration.S.indexOf(position_prev) + 1
//      println("\tinversePdf position for j = " + j + " with k = " + configuration.K)
      modification.position = Position(j, position)
      1.0 / configuration.K
    case _ => 0.0
  }
}