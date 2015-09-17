package fr.ign.rjmcmc4s.kernel

trait Transition {
  type Configuration
  def using(transform: Configuration => Double) = 1
}