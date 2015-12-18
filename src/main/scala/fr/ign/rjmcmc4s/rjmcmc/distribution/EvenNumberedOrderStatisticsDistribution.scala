package fr.ign.rjmcmc4s.rjmcmc.distribution

import org.apache.commons.math3.random.RandomGenerator

class EvenNumberedOrderStatisticsDistribution(val size: Int, val a: Double, val b: Double)(implicit val rng: RandomGenerator) extends Distribution[List[Double]] {
  val s = b - a
  val math = new org.apache.commons.math3.distribution.UniformRealDistribution(rng, a, b)
  def pdfRatio(n0: List[Double], n1: List[Double]) = pdf(n0) / pdf(n1)
  def pdf(n: List[Double]): Double = {
    // fold with z = (index, currentvalue, result)
    val r = n.foldLeft((2, a, 1.0))((z, current) => {
      if (current < z._2) return 0.0
      (z._1 + 2, current, (z._3 * (current - z._2) * z._1 * (z._1 + 1) / (s * s)))
    })
    if (b < r._2) return 0.0
    r._3 * (b - r._2) / s
  }
  def sample = {
    List.fill(size)(math.sample).sorted.zipWithIndex.filter { case ((v, i)) => i % 2 == 0 }.unzip._1
  }
}