package fr.ign.rjmcmc4s.mpp.kernel

import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.rjmcmc.kernel.SimpleVariate
import fr.ign.rjmcmc4s.mpp.ObjectSampler
import fr.ign.rjmcmc4s.rjmcmc.kernel.Variate

class ObjectBirth[T <: Iterable[Double]](val variate: Variate, val builder: ObjectBuilder[T])(implicit val rng: RandomGenerator) extends ObjectSampler[T] {
  def sample = builder.create(variate.compute(builder.dimension)._1)
  def pdf(t: T) = variate.pdf(t)
}