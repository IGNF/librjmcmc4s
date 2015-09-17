package fr.ign.rjmcmc4s.samples.coal.configuration

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import scala.collection.mutable.MutableList
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Birth
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Death
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Height
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalModification.Position
import org.apache.commons.math3.util.FastMath

object CoalModification {
  case class Birth(j: Int, h0: Double, h1: Double, s: Double) extends CoalModification
  case class Death(j: Int, h: Double) extends CoalModification
  case class Height(j: Int, h: Double) extends CoalModification
  case class Position(j: Int, s: Double) extends CoalModification
}
class CoalModification extends Modification {
  var birth: Birth = null
  var death: Death = null
  var height: Height = null
  var position: Position = null
  def apply(conf: Configuration) = conf match {
    case configuration: CoalConfiguration =>
      if (death != null) configuration.remove(death.j, death.h)
      if (birth != null) configuration.insert(birth.j, birth.h0, birth.h1, birth.s)
      if (height != null) configuration.H.update(height.j, height.h)
      if (position != null) configuration.S.update(position.j, position.s)
  }
}
class Likelihood(val y: Seq[Int]) {
  def logLikelihood(conf: CoalConfiguration) = {
    var result = 0.
    //    for (v <- y) result += FastMath.log(conf.getHeight(v))
    for (i <- (0 to conf.K)) {
      //      println(i + " h = " + conf.height(i) + " s(i) = " + conf.position(i) + " s(i+1) = " + conf.position(i + 1))
      result += FastMath.log(conf.height(i)) * y.count(yy => yy >= conf.position(i) && yy < conf.position(i + 1))
    }
    //    for (v <- y) {
    //      println("v = " + v + " h = " + conf.getHeight(v) + " logh = " + FastMath.log(conf.getHeight(v)))
    //    }
    //    println("A = " + result + " => " + FastMath.exp(result))
    var sum = 0.
    for (i <- (0 to conf.K)) {
      //      println(i + " h = " + conf.height(i) + " s(i) = " + conf.position(i) + " s(i+1) = " + conf.position(i + 1))
      sum += conf.height(i) * (conf.position(i + 1) - conf.position(i))
    }
    //    println("B = " + sum + " => " + FastMath.exp(sum))
    //    println("L = " + (result - sum) + " => " + FastMath.exp(result - sum))
    result - sum
  }
  def get(configuration: CoalConfiguration) = {
    val logLikelihoodValue = logLikelihood(configuration)
    logLikelihoodValue
  }
  def delta(configuration: CoalConfiguration, modification: CoalModification) = {
    //    println("   configuration = " + configuration)
    val oldLogLikelihood = configuration.getEnergy
    //    println("\toldLogLikelihood = " + oldLogLikelihood)
    val newConfiguration = new CoalConfiguration(configuration)
    modification.apply(newConfiguration)
    //    println("   newConfiguration = " + newConfiguration)
    val newLogLikelihood = newConfiguration.getEnergy
    //    println("\tnewLogLikelihood = " + newLogLikelihood)
    val result = newLogLikelihood - oldLogLikelihood
    //    println("\tdeltaLogLikelihood = " + result)
    if (modification.birth != null) {
      println("birth (" + modification.birth.s + ") " + configuration.height(modification.birth.j) + " => " + modification.birth.h0 + " " + modification.birth.h1)
      println("   old configuration\n" + configuration)
      println("   new configuration\n" + newConfiguration)
      println("   LogLikelihood = " + oldLogLikelihood + " => " + newLogLikelihood + " delta = " + result)
    }
    result
  }
}
class CoalConfiguration(val likelihood: Likelihood, val L: Double, var K: Int, var H: MutableList[Double], var S: MutableList[Double]) extends Configuration {
  def this(likelihood: Likelihood, L: Double) {
    this(likelihood, L, 0, new MutableList[Double], new MutableList[Double])
  }
  def this(c: CoalConfiguration) {
    this(c.likelihood, c.L, c.K, c.H.clone, c.S.clone)
  }
  def copy = new CoalConfiguration(this)
  def getEnergy() = likelihood.get(this)
  def deltaEnergy(modif: Modification) = modif match {
    case modification: CoalModification => likelihood.delta(this, modification)
  }
  def modification() = new CoalModification
  def insert(j: Int, h0: Double, h1: Double, s: Double) = {
    H = H.take(j) ++ List(h0, h1) ++ H.drop(j + 1)
    S = S.take(j + 1) ++ List(s) ++ S.drop(j + 1)
    K += 1
  }
  def remove(j: Int, h: Double) = {
    H = H.take(j) ++ List(h) ++ H.drop(j + 2)
    S = S.take(j + 1) ++ S.drop(j + 2)
    K -= 1
  }
  def height(index: Int) = H.apply(index)
  def position(index: Int) = if (index <= K) S.apply(index) else L

  def getHeight(x: Int): Double = {
    val index = S.lastIndexWhere(s => x >= s)
    //    if (x == L) println("L index = " + index + " H = " + H.apply(index) + " with k = " + K + " " + H.size + " " + position(index))
    H.apply(index)
    //    if (x == L) H.last
    //    if (x < position(current + 1)) H.apply(current)
    //    else getHeight(x, current + 1)
  }
  //  def getPosterior(x: Int) = {
  //    var result = 0.
  //    for (index <- (0 to K)) {
  //      result -= (position(index + 1) - position(index)) * height(index)
  //    }
  //    getHeight(x, 0) / result
  //  }
//  override def toString = "K= " + K + " H = " + H + " S = " + S + " L = " + L
  override def toString = {
    var result = "K = " + K+"\n"
    for (i <- (0 to K)) {
      result += "" + position(i) + "; " + height(i) + "; " + (position(i + 1) - position(i)) + "\n"
//      sum += conf.height(i) * (conf.position(i + 1) - conf.position(i))
    }
    result
  }
}