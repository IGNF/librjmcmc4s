package fr.ign.rjmcmc4s.rjmcmc.visitor

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler

class BestVisitor extends Visitor {
  var bestConfiguration: Configuration = null
  var bestEnergy: Double = Double.PositiveInfinity
  def init(dump: Int, save: Int) {
  }
  def begin(config: Configuration, sampler: Sampler) {
  }
  def visit(config: Configuration, sampler: Sampler) {
    val energy = config.getEnergy
    if (energy < bestEnergy) {
      bestEnergy = energy
      bestConfiguration = config.copy
    }
  }
  def end(config: Configuration, sampler: Sampler) {
  }
}