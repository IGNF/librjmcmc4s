package fr.ign.rjmcmc4s.simulatedannealing.schedule

import fr.ign.rjmcmc4s.simulatedannealing.temperature.Temperature

//class GeometricSchedule[T <: Temperature](var temperature: T, val alpha: Double) extends Schedule[T] {
//  def getTemperature() = this.temperature
//  def next() = {
//    this.temperature.cool(alpha);
//    this
//  }
//}

class GeometricSchedule(var temperature: Double, val alpha: Double) extends Schedule {
  def getTemperature = this.temperature
  def next = {
    this.temperature *= alpha
    this
  }
}