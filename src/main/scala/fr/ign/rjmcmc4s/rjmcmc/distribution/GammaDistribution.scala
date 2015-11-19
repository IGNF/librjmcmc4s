package fr.ign.rjmcmc4s.rjmcmc.distribution

import org.apache.commons.math3.random.RandomGenerator

class GammaDistribution(alpha: Double, beta: Double)(implicit val rng: RandomGenerator) extends Distribution[Double] {
  val math = new org.apache.commons.math3.distribution.GammaDistribution(rng, alpha, 1/beta, org.apache.commons.math3.distribution.GammaDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
  def pdfRatio(n0: Double, n1: Double) = {
    var res = 1.0
    println("pdfRatio not implemented for GammaDistribution")
    res
  }
  def pdf(n: Double) = math.density(n)
  def sample = math.sample
}