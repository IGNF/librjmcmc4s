package fr.ign.rjmcmc4s.configuration

trait Configuration {
  /**
   * Compute the energy difference caused by the application of the given modification.
   * @param m
   *        a modification
   * @return
   */
  def deltaEnergy(modif: Modification): Double

  /**
   * @return the current energy of the configuration.
   */
  def getEnergy(): Double
  def modification(): Modification
  def copy(): Configuration
}