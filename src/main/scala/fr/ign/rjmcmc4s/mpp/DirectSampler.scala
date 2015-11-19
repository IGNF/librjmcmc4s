package fr.ign.rjmcmc4s.mpp

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.sampler.Density
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.distribution.Distribution
import fr.ign.rjmcmc4s.mpp.configuration.ListBufferConfiguration
import fr.ign.rjmcmc4s.mpp.configuration.BirthDeathModification
import fr.ign.rjmcmc4s.rjmcmc.sampler.ConfigurationSampler

trait ObjectSampler[T] {
  def sample: T
  def pdf(t: T): Double
}
class DirectSampler[T](val distribution: Distribution[Int], val objectSampler: ObjectSampler[T]) extends Density with ConfigurationSampler {
  def sample(c: Configuration) = c match {
    case configuration: ListBufferConfiguration[T] => {
      configuration.clear
      val n = distribution.sample
      for (i <- 0 until n) {
        configuration.insert(objectSampler.sample)
      }
    }
    case _ => {}
  }
  def pdfRatio(c: Configuration, m: Modification) = (c, m) match {
    case (configuration: ListBufferConfiguration[T], modification: BirthDeathModification[T]) => {
      val n0 = configuration.size
      val n1 = n0 + modification.birth.size - modification.death.size
      var ratio = distribution.pdfRatio(n0, n1)
      for(b <- modification.birth) ratio *= objectSampler.pdf(b)
      for(d <- modification.death) ratio /= objectSampler.pdf(d)
      ratio
    }
    case _ => 0.0
  }
}