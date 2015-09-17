package fr.ign.rjmcmc4s.kernel

trait ReversibleKernel {
  type Configuration
  def using(transform: Configuration => Configuration)
}