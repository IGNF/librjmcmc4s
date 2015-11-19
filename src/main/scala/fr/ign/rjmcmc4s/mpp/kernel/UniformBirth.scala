package fr.ign.rjmcmc4s.mpp.kernel

import org.apache.commons.math3.random.RandomGenerator

import fr.ign.rjmcmc4s.rjmcmc.kernel.DiagonalAffineTransform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformedVariate
import fr.ign.rjmcmc4s.rjmcmc.kernel.SimpleVariate

object UniformBirth {
  def getVariate[T <: Iterable[Double]](a: T, b: T)(implicit rng: RandomGenerator) = {
    val d = a.zip(b).map(_ match {case (x,y) => y - x})
    val transform = new DiagonalAffineTransform(d, a)
    val variate = new SimpleVariate
    new TransformedVariate(transform, variate)
  }
}
class UniformBirth[T <: Iterable[Double]](builder: ObjectBuilder[T], a: T, b: T)(implicit rng: RandomGenerator) extends ObjectBirth[T](UniformBirth.getVariate(a, b), builder)(rng) {

}