package fr.ign.rjmcmc4s.simulatedannealing.temperature

trait Temperature {
  def cool(alpha: Double)
}