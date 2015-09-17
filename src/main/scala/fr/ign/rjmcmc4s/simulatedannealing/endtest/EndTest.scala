package fr.ign.rjmcmc4s.simulatedannealing.endtest

import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler
import fr.ign.rjmcmc4s.simulatedannealing.temperature.Temperature
import fr.ign.rjmcmc4s.configuration.Configuration

trait EndTest {
//	def evaluate(config: Configuration, sampler: Sampler, t: Temperature ): Boolean
  	def evaluate(config: Configuration, sampler: Sampler, t: Double): Boolean
	def stop()
}