package fr.ign.rjmcmc4s.mpp.kernel

import org.apache.commons.math3.random.RandomGenerator

import fr.ign.rjmcmc4s.rjmcmc.kernel.DiagonalAffineTransform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformedVariate
import fr.ign.rjmcmc4s.rjmcmc.kernel.SimpleVariate

object UniformBirth {
  def getVariate[T <: Iterable[Double]](rng: RandomGenerator, a: T, b: T) = {
    val d = a.zip(b).map(_ match {case (x,y) => y - x})
    val transform = new DiagonalAffineTransform(d, a)
    val variate = new SimpleVariate(rng)
    new TransformedVariate(rng)(transform, variate)
  }
}
class UniformBirth[T <: Iterable[Double]](rng: RandomGenerator, builder: ObjectBuilder[T], a: T, b: T) extends ObjectBirth[T](rng, UniformBirth.getVariate(rng, a, b), builder) {

}