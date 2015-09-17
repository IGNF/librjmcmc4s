package fr.ign.rjmcmc4s.simulatedannealing.temperature

class SimpleTemperature(var temperature: Array[Double]) extends Temperature {
  def cool(alpha: Double) {
    for (i <- 0 until this.temperature.length) this.temperature(i) *= alpha
  }
  def getTemperature(i: Integer) = this.temperature(i % this.temperature.length)
  def size = this.temperature.length
}