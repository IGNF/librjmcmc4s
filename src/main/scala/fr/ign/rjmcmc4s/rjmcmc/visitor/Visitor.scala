package fr.ign.rjmcmc4s.rjmcmc.visitor

import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler
import fr.ign.rjmcmc4s.configuration.Configuration

trait Visitor {
  def init(dump: Int, save: Int)
  def begin(config: Configuration, sampler: Sampler)
  def visit(config: Configuration, sampler: Sampler)
  def end(config: Configuration, sampler: Sampler)
}