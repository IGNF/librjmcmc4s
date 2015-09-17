package fr.ign.rjmcmc4s.simulatedannealing.schedule

import fr.ign.rjmcmc4s.simulatedannealing.temperature.Temperature

//trait Schedule[T <: Temperature] {
//  def getTemperature: T
//  def next(): Schedule[T]
//}

trait Schedule {
  def getTemperature: Double
  def next: Schedule
}