package fr.ign.rjmcmc4s.samples.coal.configuration

import scala.collection.mutable.MutableList
import scala.io.Source

import org.apache.commons.math3.random.MersenneTwister

import fr.ign.rjmcmc4s.Parameters
import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.rjmcmc.acceptance.MetropolisAcceptance
import fr.ign.rjmcmc4s.rjmcmc.distribution.GammaDistribution
import fr.ign.rjmcmc4s.rjmcmc.distribution.PoissonDistribution
import fr.ign.rjmcmc4s.rjmcmc.distribution.UniformDistribution
import fr.ign.rjmcmc4s.rjmcmc.kernel.Kernel
import fr.ign.rjmcmc4s.rjmcmc.kernel.NullVariate
import fr.ign.rjmcmc4s.rjmcmc.kernel.SimpleVariate
import fr.ign.rjmcmc4s.rjmcmc.sampler.Sampler
import fr.ign.rjmcmc4s.samples.coal.kernel.BirthDeathTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.BirthView
import fr.ign.rjmcmc4s.samples.coal.kernel.DeathView
import fr.ign.rjmcmc4s.samples.coal.kernel.HeightTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.HeightView
import fr.ign.rjmcmc4s.samples.coal.kernel.PositionTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.PositionView
import fr.ign.rjmcmc4s.samples.coal.sampler.CoalSampler
import fr.ign.rjmcmc4s.samples.coal.visitor.FileVisitor

object CoalConfigurationTest extends App {
  println("CoalConfigurationTest")
  def leapyear(year: Int) = if (year % 4 != 0) false else if (year % 100 != 0) true else (year % 400 == 0)
  def numberofdays(year: Int) = if (leapyear(year)) 366.0 else 365.0
  def difference(d1: Double, d2: Double): Int = {
    val y1 = d1.intValue
    val y2 = d2.intValue
    if (y1 == y2) ((d2 - d1) * numberofdays(y1)).round.toInt
    else {
      val d = d1.intValue + 1
      ((d - d1) * numberofdays(y1)).round.toInt + difference(d, d2)
    }
  }
  val src = Source.fromFile("./src/main/resources/coal.csv")
  val iter = src.getLines().drop(1).map(_.split(",").apply(1).toDouble)
  var current = iter.next
  val res = iter.map(x => {
    val previous = current
    current = x
    difference(previous, current)
  }).toSeq
  //  res foreach (println _)

  val s = res.foldLeft((0, Seq(0)))((x, v) => x match { case (s, l) => (s + v, l ++ Seq(s + v)) })
  val lastValue = s._1
  val y = s._2
  //  y foreach (println _)

  val parameters = new Parameters("./src/main/resources/coal_parameters.xml")

  val L = lastValue
  val Kmax = parameters.getInt("kmax")
  val lambda = parameters.getDouble("lambda")
  val alpha = parameters.getDouble("alpha")
  val beta = parameters.getDouble("beta")
  val burnin = parameters.getInt("burnin")
  val updates = parameters.getInt("updates")
  println("L = " + L)
  println("Kmax = " + Kmax)
  println("Lambda = " + lambda)
  println("Alpha = " + alpha)
  println("Beta = " + beta)
  implicit val rng = new MersenneTwister(0)

  var configuration = new CoalConfiguration(new Likelihood(y), L)
  configuration.K = 2
  configuration.S += 0.0
  configuration.S += 14420
  configuration.S += 35240
  configuration.H += 0.009 //1.0
  configuration.H += 0.003 //1.0
  configuration.H += 0.001 //1.0
  //  configuration.K = 1
  //  configuration.S += 0.0
  //  configuration.S += 14420
  //  configuration.H += 0.009//1.0
  //  configuration.H += 0.003//1.0

  println("config = " + configuration)
  println("energy = " + configuration.getEnergy)

  val KDistribution = new PoissonDistribution(rng, lambda, Kmax)
  val HDistribution = new GammaDistribution(rng, alpha, beta)
  val SDistribution = new UniformDistribution(rng, 0.0, L)
  val acceptance = new MetropolisAcceptance
  val density = new CoalSampler(rng, KDistribution, HDistribution, SDistribution)
  val variate = new SimpleVariate(rng)

  var c: Double = Math.min(1.0 / Math.min(1, KDistribution.pdf(1) / KDistribution.pdf(0)), 1.0 / Math.min(1, KDistribution.pdf(Kmax - 1) / KDistribution.pdf(Kmax)))
  for (k <- 1 to Kmax - 1)
    c = Math.min(c, 1.0 / Math.min(1.0, KDistribution.pdf(k + 1) / KDistribution.pdf(k)) + 1.0 / Math.min(1, KDistribution.pdf(k - 1) / KDistribution.pdf(k)))
  c = 0.9 * c
  println("c = " + c)
  def birthK(k: Int) = if (k == Kmax) 0.0 else c * Math.min(1.0, KDistribution.pdfRatio(k + 1, k))
  def deathK(k: Int) = if (k == 0) 0.0 else c * Math.min(1.0, KDistribution.pdfRatio(k - 1, k))

  val birthDeath: Configuration => Double = c => c match {
    case config: CoalConfiguration => birthK(config.K) + deathK(config.K)
  }
  val birth_choice: Configuration => Double = c => c match {
    case config: CoalConfiguration => if (config.K == 0) 1.0 else if (config.K == Kmax) 0.0 else {
      val ratio = KDistribution.pdf(config.K + 1) / (KDistribution.pdf(config.K + 1) + KDistribution.pdf(config.K - 1))
      //      println("k = " + config.K + " choice " + ratio)
      Math.min(1.0, ratio)
    }
  }
  val birth_ratio: (Boolean, Configuration) => Double = (d, c) => (d, c) match {
    case (true, config: CoalConfiguration) => if (config.K == 0) 1.0 else KDistribution.pdfRatio(config.K, config.K + 1)
    case (false, config: CoalConfiguration) => if (config.K == Kmax) 1.0 else KDistribution.pdfRatio(config.K, config.K - 1)
    case _ => 0.0
  }
  val birthdeathKernel = new Kernel(new BirthView(rng), new DeathView(rng), variate, NullVariate, new BirthDeathTransform, birthDeath, birth_ratio)
  birthdeathKernel.name = "BirthDeath"

  val height: Configuration => Double = c => c match {
    case config: CoalConfiguration => if (config.K == 0) (1.0 - (birthK(config.K) + deathK(config.K))) else (1.0 - (birthK(config.K) + deathK(config.K))) / 2.0
  }
  val heightKernel = new Kernel(new HeightView(rng), new HeightView(rng), variate, variate, new HeightTransform, height)
  heightKernel.name = "Height"

  //  val position: Configuration => Double = c => c match { case config: CoalConfiguration => if (config.K == 0) 0. else 1. }
  val position: Configuration => Double = c => c match {
    case config: CoalConfiguration => if (config.K == 0) 0.0 else height(c)
  }

  val positionKernel = new Kernel(new PositionView(rng), new PositionView(rng), variate, variate, new PositionTransform, position)
  positionKernel.name = "Position"

  val kernels: Seq[Kernel] = List( birthdeathKernel, heightKernel, positionKernel)
  // new RJMCMC Sampler
  val sampler = new Sampler(density, acceptance, kernels)
  val temp = -1
  val list = new MutableList[CoalConfiguration]
  val visitor = new FileVisitor("visitor.txt")
  visitor.init(1, 0)
  visitor.begin(configuration, sampler)
  visitor.visit(configuration, sampler)
  list += new CoalConfiguration(configuration)
  val numberOfSamples = 9999
  for (i <- 0 until numberOfSamples) {
    sampler.sample(configuration, temp)
    visitor.visit(configuration, sampler)
    val copy = new CoalConfiguration(configuration)
    println(copy.getEnergy)
    list += copy
  }
  visitor.end(configuration, sampler)
  println("all done")

  val maxK = list.maxBy(c => c.K)
  println("max K = " + maxK.K)

  println("k;n")
  for (kk <- 0 to maxK.K) {
    val count = list.count(c => c.K == kk)
    println(kk + "; " + count)
  }
  val bestConfig = list.maxBy(c => c.getEnergy)
  println(bestConfig)
  println(bestConfig.getEnergy)
  var best: CoalConfiguration = null
  var bestValue = Double.NegativeInfinity
  list.foreach(c => {
    val e = c.getEnergy
    //    println(e)
    if (e > bestValue) {
      best = c
      bestValue = e
    }
  })
  println("best = " + best)
  println("energy = " + best.getEnergy + " == " + bestValue)
  for (kk <- 0 to best.K) {
    val count = y.count(v => v >= best.position(kk) && v < best.position(kk + 1))
    println(count + " btw " + best.position(kk) + " and " + best.position(kk + 1))
  }
}