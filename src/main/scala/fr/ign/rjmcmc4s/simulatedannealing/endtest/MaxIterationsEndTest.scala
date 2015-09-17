package fr.ign.rjmcmc4s.simulatedannealing.endtest

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler

class MaxIterationsEndTest(var iterations: Integer) extends EndTest {
//  def evaluate(config: Configuration, sampler: Sampler, t: Temperature) {
  def evaluate(config: Configuration, sampler: Sampler, t: Double) = {
    this.iterations -= 1
    this.iterations <= 0
  }
  def stop {
    this.iterations = 0;
  }
//  @Override
//  def toString = "" + this.iterations
}