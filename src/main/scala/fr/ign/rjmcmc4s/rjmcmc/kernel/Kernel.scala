package fr.ign.rjmcmc4s.rjmcmc.kernel

import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.configuration.Configuration
import scala.collection.mutable.MutableList

class KernelResult(val ratio: Double, val modif: Modification)

class Kernel(val name: String, val view0: View, val view1: View, val variate0: Variate, val variate1: Variate, val transform: Transform,
  val probability: Configuration => Double = (_ => 1.0),
  //    val q: Configuration => Double = (_ => .5),
  val proposal_ratio: (Boolean, Configuration) => Double = (_, _) => 1.0) {
  var kernelId: Int = 0
  def apply(p: Double, c: Configuration, m: Modification): Double = {
    val prob = this.probability(c)
    //    val q = this.q(c)
    val proposal0 = this.proposal_ratio(true, c)
    val proposal1 = this.proposal_ratio(false, c)
    //    val p01 = q * prob
    val p01 = prob * proposal0 / (proposal0 + proposal1)
    //        println("p =" + p)
    //        println("p01=" + p01)
    //    val p10 = prob - p01
    //        println("p10=" + p10)
//    println("weight = " + prob + " choice = " + p01 + " " + (p01 / prob))
//    println("ratios = " + this.proposal_ratio(true, c) + " - " + this.proposal_ratio(false, c))
    if (p < p01) { // branch probability : m_p01/m_p
      this.kernelId = 0;
//      println(this.name + "_" + this.kernelId)
      var val0 = new MutableList[Double]
      val J01 = view0.pdf(c, m, val0) // returns the discrete probability that samples the portion of the configuration that is being modified (stored in the modif input)
      if (J01 == 0) return 0.0 // abort : view sampling failed
      val size = this.transform.size - val0.size
      val (var0, phi01) = this.variate0.compute(size)
      //                val phi01 = variate(var0);             // returns the continuous probability that samples the completion variates
      if (phi01 == 0) return 0; // abort : variate sampling failed
      val trans = this.transform.apply(true, val0 ++ var0) // computes val1 from val0
      val jacob = trans.jacob
      val val1 = trans.output.take(view1.dimension)
      val var1 = trans.output.drop(view1.dimension)
      val phi10 = variate1.pdf(var1) // returns the continuous probability of the variate sampling, arguments are constant
      val J10 = this.view1.inversePdf(c, m, val1); // returns the discrete probability of the inverse view sampling, arguments are constant except val1 that is encoded in modif
      val proposal = J10 * phi10 / (J01 * phi01)
      val ratio = this.proposal_ratio(true, c)
//      println("\t\tjacob = " + jacob + " proposal  = " + (proposal * ratio))
      val v = jacob * (proposal * ratio)
      //      println("v0=" + jacob + " * " + ratio + " * ( " + J10 + " * " + phi10 + " ) / ( " + J01 + " * " + phi01 + " ))")
      v
    } else { // branch probability : m_p10/m_p
      this.kernelId = 1
//      println(this.name + "_" + this.kernelId)
      var val1 = new MutableList[Double]
      val J10 = view1.pdf(c, m, val1) // returns the discrete probability that samples the portion of the configuration that is being modified (stored in the modif input)
      if (J10 == 0) return 0.0 // abort : view sampling failed
      val size = this.transform.size - val1.size
      val (var1, phi10) = this.variate1.compute(size)
      //      println("val1" + val1)
      //      println("var1" + var1)
      //                val phi10 = m_variate1(e,var1);      // returns the continuous probability that samples the completion variates
      if (phi10 == 0) return 0; // abort : variate sampling failed
      val trans = this.transform.apply(false, val1 ++ var1) // computes val1 from val0
      val jacob = trans.jacob
      val val0 = trans.output.take(view0.dimension)
      val var0 = trans.output.drop(view0.dimension)
      val phi01 = this.variate0.pdf(var0); // returns the continuous probability of the inverse variate sampling, arguments are constant
      val J01 = this.view0.inversePdf(c, m, val0); // returns the discrete probability of the inverse view sampling, arguments are constant except val1 that is encoded in modif
      val proposal = J01 * phi01 / (J10 * phi10)
      val ratio = this.proposal_ratio(false, c)
//      println("\t\tjacob = " + jacob + " proposal  = " + (proposal * ratio))
      val v = jacob * (proposal * ratio)
      //      println("v1=" + jacob + " * ( " + J01 + " * " + phi01 + " ) / ( " + ratio + " * " + J10 + " * " + phi10 + " ))")
      v
    }
  }
}