package fr.ign.rjmcmc4s.rjmcmc.sampler

import org.apache.commons.math3.random.RandomGenerator
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.rjmcmc.kernel.Kernel
import fr.ign.rjmcmc4s.rjmcmc.kernel.KernelResult
import org.apache.commons.math3.util.FastMath
import com.typesafe.scalalogging.LazyLogging

trait Density {
  def pdfRatio(c: Configuration, m: Modification): Double
}

trait ConfigurationSampler {
  def sample(c: Configuration)
}

trait Acceptance {
  def eval(delta: Double, temp: Double, greenRatio: Double): Double
}

class Sampler(val density: Density, val acceptance: Acceptance, val kernels: Seq[Kernel]) extends LazyLogging {
  var acceptance_probability: Double = 0.0
  var temperature: Double = 0.0
  var delta: Double = 0.0
  var green_ratio: Double = 0.0
  var kernel_ratio: Double = 0.0
  var ref_pdf_ratio: Double = 0.0
  var accepted: Boolean = false
  var kernel_id: Int = 0

  def sample(c: Configuration, temp: Double = -1.0)(implicit rng: RandomGenerator): Unit = {
    class KernelFunctor(val c: Configuration, m: Modification) {
      def apply(x: Double, k: Kernel) = k.apply(x, c, m)
    }
    def randomApplyImpl(x: Double, index: Int, kernels: Seq[Kernel], kf: KernelFunctor): (Double, Int) = {
      val y = x - kernels.head.probability(kf.c)
      if (y > 0) randomApplyImpl(y, index + 1, kernels.tail, kf)
      else (kf.apply(x, kernels.head), index)
    }
    def randomApply(x: Double, kernels: Seq[Kernel], kf: KernelFunctor) = {
      val normalisation = kernels.foldLeft(0.0)((x, y) => x + y.probability(kf.c))
      randomApplyImpl(x * normalisation, 0, kernels, kf)
    }
    this.temperature = temp
    val modif = c.modification
    val kf = new KernelFunctor(c, modif)
    val apply = randomApply(rng.nextDouble(), this.kernels, kf)
    this.kernel_ratio = apply._1
    this.kernel_id = apply._2
    this.ref_pdf_ratio = density.pdfRatio(c, modif)
    this.green_ratio = kernel_ratio * ref_pdf_ratio
    logger.debug(s"\t\tsampler: green_ratio = $green_ratio = kernel_ratio = $kernel_ratio * prior_ratio = $ref_pdf_ratio")
    if (this.green_ratio <= 0) {
      this.delta = 0;
      this.accepted = false;
      return
    }
    this.delta = c.deltaEnergy(modif)
    this.acceptance_probability = FastMath.min(1.0, acceptance.eval(this.delta, this.temperature, this.green_ratio))
    logger.debug(s"PROPOSED $this.kernel_id\n\tdelta = $delta acceptance_probability = $acceptance_probability")
    this.accepted = (rng.nextDouble() < this.acceptance_probability);
    if (this.accepted) {
      logger.debug(s"ACCEPTED $this.kernel_id\n\tdelta = $delta acceptance_probability = $acceptance_probability")
      modif.apply(c);
    }
  }
}