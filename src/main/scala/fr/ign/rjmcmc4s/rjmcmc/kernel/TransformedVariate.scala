package fr.ign.rjmcmc4s.rjmcmc.kernel

import org.apache.commons.math3.random.RandomGenerator

/**
 * A Variate with a <code>Transform</code>.
 * @author Julien Perret
 */
class TransformedVariate(val transform: Transform, val variate: SimpleVariate)(implicit val rng: RandomGenerator) extends Variate {
  override def compute(size: Int): (Seq[Double], Double) = {
    val res = variate.compute(size)
    val trans = transform.apply(true, res._1)
    (trans.output.toSeq, res._2 / trans.jacob)
  }
  override def pdf(it: Iterable[Double]) = {
    val trans = transform.apply(false, it)
    trans.jacob * variate.pdf(trans.output)
  }

}