package fr.ign.rjmcmc4s.simulatedannealing

import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler
import fr.ign.rjmcmc4s.rjmcmc.visitor.Visitor
import fr.ign.rjmcmc4s.simulatedannealing.endtest.EndTest
import fr.ign.rjmcmc4s.simulatedannealing.schedule.Schedule
import fr.ign.rjmcmc4s.simulatedannealing.temperature.Temperature

object SimulatedAnnealing {
  //  def optimize[T <: Temperature](rng: RandomGenerator, config: Configuration, sampler: Sampler,
  def optimize(rng: RandomGenerator, config: Configuration, sampler: Sampler, schedule: Schedule, endTest: EndTest, visitor: Visitor) {
    var t = schedule.getTemperature
    if (visitor != null) visitor.begin(config, sampler) //TODO ADD T TO VISITOR ?
    while (!endTest.evaluate(config, sampler, t)) {
      sampler.sample(config, t)(rng)
      if (visitor != null) visitor.visit(config, sampler) //TODO ADD T TO VISITOR ?
      t = schedule.next.getTemperature
    }
    if (visitor != null) visitor.end(config, sampler) //TODO ADD T TO VISITOR ?
  }
}