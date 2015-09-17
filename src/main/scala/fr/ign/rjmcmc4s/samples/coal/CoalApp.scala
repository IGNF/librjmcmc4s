package fr.ign.rjmcmc4s.samples.coal

import java.io.File
import java.io.PrintWriter

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
import fr.ign.rjmcmc4s.samples.coal.configuration.CoalConfiguration
import fr.ign.rjmcmc4s.samples.coal.configuration.Likelihood
import fr.ign.rjmcmc4s.samples.coal.kernel.BirthDeathTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.BirthView
import fr.ign.rjmcmc4s.samples.coal.kernel.DeathView
import fr.ign.rjmcmc4s.samples.coal.kernel.HeightTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.HeightView
import fr.ign.rjmcmc4s.samples.coal.kernel.PositionTransform
import fr.ign.rjmcmc4s.samples.coal.kernel.PositionView
import fr.ign.rjmcmc4s.samples.coal.sampler.CoalSampler
import fr.ign.rjmcmc4s.samples.coal.visitor.FileVisitor

object CoalApp extends App {
  //  def leapyear(year: Int) = if (year % 4 != 0) false else if (year % 100 != 0) true else (year % 400 == 0)
  //  def numberofdays(year: Int) = if (leapyear(year)) 366. else 365.
  //  def difference(d1: Double, d2: Double): Int = {
  //    val y1 = d1.intValue
  //    val y2 = d2.intValue
  //    if (y1 == y2) ((d2 - d1) * numberofdays(y1)).round.toInt
  //    else {
  //      val d = d1.intValue + 1
  //      ((d - d1) * numberofdays(y1)).round.toInt + difference(d, d2)
  //    }
  //  }
  //  val src = Source.fromFile("./src/main/resources/coal.csv")
  //  val iter = src.getLines().drop(1).map(_.split(",").apply(1).toDouble)
  //  var current = iter.next
  //  val res = iter.map(x => {
  //    val previous = current
  //    current = x
  //    difference(previous, current)
  //  }).toSeq
  //  //  res foreach (println _)
  //
  //  val s = res.foldLeft((0, Seq(0)))((x, v) => x match { case (s, l) => (s + v, l ++ Seq(s + v)) })
  //  val lastValue = s._1
  //  val y = s._2
  //  y foreach (println _)

  val y = Source.fromFile("./src/main/resources/coal_green.csv").getLines.map(x => x.toInt).toSeq
  val parameters = new Parameters("./src/main/resources/coal_parameters.xml")

  val L = parameters.getInt("L")
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
  implicit val rng = new MersenneTwister(1)
  val KDistribution = new PoissonDistribution(rng, lambda, Kmax)
  val HDistribution = new GammaDistribution(rng, alpha, beta)
  val SDistribution = new UniformDistribution(rng, 0., L)
  val acceptance = new MetropolisAcceptance
  val density = new CoalSampler(rng, KDistribution, HDistribution, SDistribution)
  var configuration = new CoalConfiguration(new Likelihood(y), L)
  density.sample(configuration)
  val variate = new SimpleVariate(rng)

  //  val c: Double = 9.
  //  val birthK: Configuration => Double = conf => conf match {
  //    case config: CoalConfiguration => if (config.K == Kmax) 0. else c * Math.min(1., KDistribution.pdfRatio(config.K + 1, config.K))
  //  }
  //  val deathK: Configuration => Double = conf => conf match {
  //    case config: CoalConfiguration => if (config.K == 0) 0. else c * Math.min(1., KDistribution.pdfRatio(config.K - 1, config.K))
  //  }
  val birthDeath: Configuration => Double = c => 9.
  //  val birth_choice: Configuration => Double = c => c match {
  //    case config: CoalConfiguration => if (config.K == 0) 1. else if (config.K == Kmax) 0. else {
  //      val ratio = KDistribution.pdf(config.K + 1) / (KDistribution.pdf(config.K + 1) + KDistribution.pdf(config.K - 1))
  //      println("k = " + config.K + " choice " + ratio)
  //      Math.min(1., ratio)
  //    }
  //  }
  val birth_ratio: (Boolean, Configuration) => Double = (d, c) => (d, c) match {
    case (true, config: CoalConfiguration) => if (config.K == 0) 1. else KDistribution.pdfRatio(config.K, config.K + 1)
    case (false, config: CoalConfiguration) => if (config.K == Kmax) 1. else KDistribution.pdfRatio(config.K, config.K - 1)
    case _ => 0.
  }
  val birthdeathKernel = new Kernel(new BirthView(rng), new DeathView(rng), variate, NullVariate, new BirthDeathTransform, birthDeath, /*birth_choice, */ birth_ratio)
  birthdeathKernel.name = "BirthDeath"

  val height: Configuration => Double = c => 1.
  val heightKernel = new Kernel(new HeightView(rng), new HeightView(rng), variate, variate, new HeightTransform, height)
  heightKernel.name = "Height"

  val position: Configuration => Double = c => c match { case config: CoalConfiguration => if (config.K == 0) 0. else 1. }
  val positionKernel = new Kernel(new PositionView(rng), new PositionView(rng), variate, variate, new PositionTransform, position)
  positionKernel.name = "Position"

  val kernels: Seq[Kernel] = List(birthdeathKernel /*, heightKernel, positionKernel*/ )
  // new RJMCMC Sampler
  val sampler = new Sampler(density, acceptance, kernels)
  println("start burnin")
  val temp = -1
  for (i <- 0 until burnin) {
    sampler.sample(configuration, temp)
  }
  println("burnin done")
  val list = new MutableList[CoalConfiguration]
  val visitor = new FileVisitor("visitor.txt")
  visitor.init(1000, 0)
  visitor.begin(configuration, sampler)
  for (i <- 0 until updates) {
    sampler.sample(configuration, temp)
    visitor.visit(configuration, sampler)
    val copy = new CoalConfiguration(configuration)
    //    println("e = " + copy.getEnergy)
    list += copy
  }
  visitor.end(configuration, sampler)
  println("all done")

  val maxK = list.maxBy(c => c.K)
  println("max K = " + maxK.K)
  var writer = new PrintWriter(new File("k.txt"))
  for (kk <- (0 to maxK.K)) {
    val count = list.count(c => c.K == kk)
    println("" + kk + "; " + count.toDouble / updates.toDouble)
    writer.write(kk + "; " + count.toDouble / updates.toDouble + "\n")
  }
  writer.close
  val bestConfig = list.maxBy(c => c.getEnergy)
  println("best = " + bestConfig)
  println("best energy = " + bestConfig.getEnergy)

  writer = new PrintWriter(new File("result.txt"))
  //  var meanrate = MutableList.fill(L + 1)(0.0)
  //  for (conf <- list) {
  //    for (kk <- (0 to conf.K)) {
  //      val h = conf.height(kk)
  //      for (i <- conf.position(kk).toInt to conf.position(kk + 1).toInt) {
  //        meanrate(i) += h
  //      }
  //    }
  //  }
  for (x <- (0 to L)) {
    //    val s = meanrate(x) / updates
    val s = list.foldLeft(0.)((s, c) => s + c.getHeight(x)) / updates
    //    var sum = 0.
    //    for (c <- list) {
    //      sum += FastMath.exp(c.getPosterior(x))
    //    }
    writer.write("" + s + "\n")
  }
  writer.close
}