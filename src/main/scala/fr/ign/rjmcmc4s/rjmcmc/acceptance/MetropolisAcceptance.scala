package fr.ign.rjmcmc4s.rjmcmc.acceptance

import fr.ign.rjmcmc4s.rjmcmc.sampler.Acceptance
import org.apache.commons.math3.util.FastMath

class MetropolisAcceptance extends Acceptance {
  /**
   * \ingroup GroupAcceptance
   *
   * This class is a model of the Acceptance concept and implements <i>Metropolis</i> acceptance rule. The new state is accepted with probability:
   * \f[P_{Metropolis}=\min\left(1,\exp\left(-\Delta E / T\right)\right)\f]
   */
  def eval(delta: Double, temperature: Double, greenRatio: Double) = {
    if (temperature == 0.0) {
      if (delta > 0) {
        0.0
      }
      else {
        if (delta < 0) {
          1.0
        }
        else greenRatio
      }
    } else {
      greenRatio * FastMath.exp(-delta / temperature)
    }
  }
}