package fr.ign.rjmcmc4s.rjmcmc.kernel

trait Variate {
  def compute(size: Int): (Seq[Double], Double)
  def pdf(it: Iterable[Double]): Double
}