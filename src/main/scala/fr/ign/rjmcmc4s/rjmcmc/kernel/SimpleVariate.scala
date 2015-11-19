package fr.ign.rjmcmc4s.rjmcmc.kernel

import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.random.RandomGenerator

/**
 * A variate.
 * @author Julien Perret
 */
class SimpleVariate(implicit rng: RandomGenerator) extends Variate {
  val rand = new UniformRealDistribution(rng, 0, 1);
  /**
   * Returns the continuous probability that samples the completion variates.
   * @param size the size of the variate
   * @return the continuous probability that samples the completion variates.
   */
  override def compute(size: Int): (Seq[Double], Double) = ((0 until size) map (_ => { rand.sample() }), 1.0)

  /**
   * Returns the continuous probability of the variate sampling, arguments are constant.
   * @param var1
   *        a variate
   * @return the continuous probability of the variate sampling, arguments are constant.
   */
  override def pdf(it: Iterable[Double]) = if (it.exists(v => (v < 0 || v > 1))) 0.0 else 1.0
}