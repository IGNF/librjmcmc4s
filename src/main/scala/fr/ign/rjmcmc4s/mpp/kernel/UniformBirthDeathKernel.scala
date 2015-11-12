package fr.ign.rjmcmc4s.mpp.kernel

import fr.ign.rjmcmc4s.rjmcmc.kernel.Kernel
import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.rjmcmc.kernel.NullVariate
import fr.ign.rjmcmc4s.rjmcmc.kernel.IdentityTransform
import fr.ign.rjmcmc4s.rjmcmc.kernel.NullView
import fr.ign.rjmcmc4s.configuration.Configuration

object UniformBirthDeathKernel {
  def makeUniformBirthDeathKernel[T <: Iterable[Double]](rng: RandomGenerator, b: ObjectBirth[T], p: Configuration=> Double, q: (Boolean, Configuration) => Double = (_, _) => 1.0) = {
    val view1 = new UniformView(rng, 1, b.builder)
    new Kernel("BirthDeath", NullView, view1, b.variate, NullVariate, new IdentityTransform(view1.dimension), p, q)
  }
}