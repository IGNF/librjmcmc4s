package fr.ign.rjmcmc4s.samples.coal.sampler

import scala.collection.mutable.MutableList

import org.apache.commons.math3.random.RandomGenerator

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.distribution.Distribution
import fr.ign.rjmcmc4s.rjmcmc.sampler.ConfigurationSampler
import fr.ign.rjmcmc4s.rjmcmc.sampler.Density
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification

class CoalSampler(val rng: RandomGenerator, val KDistribution: Distribution[Int], val HDistribution: Distribution[Double], val SDistribution: Distribution[Double]) extends Density with ConfigurationSampler {
  def EvenNumberedOrderStatistics(N: Int, min: Double, max: Double): MutableList[Double] = {
    val A = MutableList.fill(N)(rng.nextDouble * (max - min) + min)
    val sortedWithIndex = A.sorted.zipWithIndex
    sortedWithIndex.collect { case (i, idx) if (idx + 1) % 2 == 0 => i }
  }
  def sample(c: Configuration) = c match {
    case configuration: CoalConfiguration => {
      configuration.K = KDistribution.sample
      configuration.H = MutableList.fill(configuration.K + 1)(HDistribution.sample)
      configuration.S = MutableList[Double](0.) ++ EvenNumberedOrderStatistics(2 * configuration.K + 1, 0., configuration.L)
    }
  }
  def pdfRatio(c: Configuration, m: Modification): Double = (c, m) match {
    case (configuration: CoalConfiguration, modification: CoalModification) => {
      var ratio = 1.0
      val k = configuration.K
      if (modification.birth != null) {
        ratio = KDistribution.pdfRatio(k, k + 1)
        println("birth k ratio = " + ratio)
        println("birth k ratio = " + (KDistribution.pdf(k + 1) / KDistribution.pdf(k)))
        val L = configuration.L
        val j = modification.birth.j
        val ss = modification.birth.s
        val s0 = configuration.position(j)
        val s1 = configuration.position(j + 1)
        val h = configuration.height(j)
        val hh0 = modification.birth.h0
        val hh1 = modification.birth.h1
        val positionRatio = (2 * (k + 1) * (2 * k + 3)) / (L * L)
        val widthRatio = ((ss - s0) * (s1 - ss)) / (s1 - s0)
        val heightRatio = (HDistribution.pdf(hh0) * HDistribution.pdf(hh1)) / HDistribution.pdf(h)
        ratio *= positionRatio
        ratio *= widthRatio
        ratio *= heightRatio
        println("positionRatio = " + positionRatio)
        println("widthRatio = " + widthRatio)
        println("heightRatio = " + heightRatio)
        println("\tbirth prior ratio " + ratio)
      }
      if (modification.death != null) {
        ratio = KDistribution.pdfRatio(k, k - 1)
        //        println("death k ratio = " + ratio)
        //        println("death k ratio = " + (KDistribution.pdf(k - 1) / KDistribution.pdf(k)))
        val L = configuration.L
        val j = modification.death.j
        val h = modification.death.h
        val h0 = configuration.height(j)
        val h1 = configuration.height(j + 1)
        val s0 = configuration.position(j)
        val s1 = configuration.position(j + 1)
        val s2 = configuration.position(j + 2)
        ratio *= HDistribution.pdf(h) / (HDistribution.pdf(h0) * HDistribution.pdf(h1))
        //        println("\tdeath prior ratio - j = " + j + " h0 = " + h0 + " h1 = " + h1 + " s0 = " + s0 + " s1 = " + s1 + " s2 = " + s2 + " h' = " + h)
        ratio *= (L * L) / (2. * k * (2. * k + 1.))
        ratio *= (s2 - s0) / ((s1 - s0) * (s2 - s1))
        //        ratio = 1 / ratio
        //        ratio *= L * L * (s2 - s0)
        //        ratio /= 2 * (k + 1) * (2 * k + 3) * (s1 - s0) * (s2 - s1)
        //        println("\tdeath prior ratio " + ratio)
      }
      if (modification.position != null) {
        //        ratio *= SDistribution.pdf(modification.position.s) / SDistribution.pdf(configuration.position(modification.position.j))
        val j = modification.position.j
        val position_prev = configuration.position(j - 1)
        val position = configuration.position(j)
        val position_next = configuration.position(j + 1)
        val newPosition = modification.position.s
        ratio = (position_next - newPosition) * (newPosition - position_prev) / ((position_next - position) * (position - position_prev))
        //        println("\tposition prior ratio " + ratio)
      }
      if (modification.height != null) {
        val h = configuration.height(modification.height.j)
        val hh = modification.height.h
        ratio = HDistribution.pdf(hh) / HDistribution.pdf(h)
        //        println("\theight prior ratio " + ratio + " h = " + h + " h' = " + hh)
      }
      ratio
    }
  }
}
