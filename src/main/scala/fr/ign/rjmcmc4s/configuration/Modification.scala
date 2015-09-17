package fr.ign.rjmcmc4s.configuration

abstract class Modification {
  def apply(conf: Configuration)
//  def +(m: Modification): Modification
}