package fr.ign.rjmcmc4s.rjmcmc.distribution

import org.apache.commons.math3.random.RandomGenerator

class PoissonDistribution(mean: Double, max: Int)(implicit val rng: RandomGenerator) extends Distribution[Int] {
  val math = new org.apache.commons.math3.distribution.PoissonDistribution(rng, mean, org.apache.commons.math3.distribution.PoissonDistribution.DEFAULT_EPSILON, org.apache.commons.math3.distribution.PoissonDistribution.DEFAULT_MAX_ITERATIONS)
  def pdfRatio(n0: Int, n1: Int) = {
    var res = 1.0
    if (n1 > n0) for (n <- n0 + 1 to n1) res *= math.getMean / n
    if (n0 > n1) for (n <- n1 + 1 to n0) res *= n / math.getMean
    res
  }
  def pdf(n: Int) = math.probability(n)
  def sample = {
    var res = math.sample
    while (res > max) res = math.sample
    res
  }
}