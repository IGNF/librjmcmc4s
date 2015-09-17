package fr.ign.rjmcmc4s.rjmcmc.kernel

object NullVariate extends Variate {
  override def compute(size: Int): (Seq[Double], Double) = (Seq[Double](), 1.)
  override def pdf(it: Iterable[Double]) = 1.
}