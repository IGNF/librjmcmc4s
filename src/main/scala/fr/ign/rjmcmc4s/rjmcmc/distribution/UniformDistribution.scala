package fr.ign.rjmcmc4s.rjmcmc.distribution

import org.apache.commons.math3.random.RandomGenerator

class UniformDistribution(min: Double, max: Double)(implicit val rng: RandomGenerator) extends Distribution[Double] {
  val math = new org.apache.commons.math3.distribution.UniformRealDistribution(rng, min, max)
  def pdfRatio(n0: Double, n1: Double) = {
    var res = 1.0
    println("pdfRatio not implemented for UniformDistribution")
    res
  }
  def pdf(n: Double) = math.density(n)
  def sample = math.sample
}