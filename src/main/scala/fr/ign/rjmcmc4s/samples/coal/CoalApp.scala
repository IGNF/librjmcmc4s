package fr.ign.rjmcmc4s.samples.coal

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable.MutableList
import scala.io.Source

import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator

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

class CoalApp(val y: Seq[Int], val L: Int, val Kmax: Int, val lambda: Double, val alpha: Double, val beta: Double, val burnin: Int, val updates: Int) {
  def apply(implicit rng: RandomGenerator): MutableList[CoalConfiguration] = {
//    val KDistribution = new PoissonDistribution(rng, lambda, Kmax)
//    val HDistribution = new GammaDistribution(rng, alpha, beta)
//    val SDistribution = new UniformDistribution(rng, 0.0, L)
    val acceptance = new MetropolisAcceptance
    val density = new CoalSampler(lambda, Kmax, alpha, beta, L)
    var configuration = new CoalConfiguration(new Likelihood(y), L)
    density.sample(configuration)
    val variate = new SimpleVariate(rng)
    val birthDeath: Configuration => Double = c => 9.0
    val birth_ratio: (Boolean, Configuration) => Double = (d, c) => (d, c) match {
      case (true, config: CoalConfiguration) => if (config.K == 0) 1.0 else density.KDistribution.pdfRatio(config.K, config.K + 1)
      case (false, config: CoalConfiguration) => if (config.K == Kmax) 1.0 else density.KDistribution.pdfRatio(config.K, config.K - 1)
    }
    val birthdeathKernel = new Kernel("BirthDeath", new BirthView(rng), new DeathView(rng), variate, NullVariate, new BirthDeathTransform, birthDeath, /*birth_choice, */ birth_ratio)
    val heightKernel = new Kernel("Height", new HeightView(rng), new HeightView(rng), variate, variate, new HeightTransform, _ => 1.0)
    val position: Configuration => Double = c => c match { case config: CoalConfiguration => if (config.K == 0) 0.0 else 1.0 }
    val positionKernel = new Kernel("Position", new PositionView(rng), new PositionView(rng), variate, variate, new PositionTransform, position)
    val kernels: Seq[Kernel] = List(birthdeathKernel, heightKernel, positionKernel)
    // new RJMCMC Sampler
    val sampler = new Sampler(density, acceptance, kernels)
    println("start burnin")
    (0 until burnin) map (_ => sampler.sample(configuration))
    println("burnin done")
    val list = new MutableList[CoalConfiguration]
    val visitor = new FileVisitor("visitor.txt")
    visitor.init(1000, 0)
    visitor.begin(configuration, sampler)
    for (i <- 0 until updates) {
      sampler.sample(configuration)
      visitor.visit(configuration, sampler)
      list += new CoalConfiguration(configuration)
    }
    visitor.end(configuration, sampler)
    println("all done")
    val maxK = list.maxBy(c => c.K)
    println("max K = " + maxK.K)
    val writer = new PrintWriter(new File("k.txt"))
    for (kk <- (0 to maxK.K)) {
      val count = list.count(c => c.K == kk)
      println("" + kk + "; " + count.toDouble / updates.toDouble)
      writer.write(kk + "; " + count.toDouble / updates.toDouble + "\n")
    }
    writer.close
    val bestConfig = list.maxBy(c => c.getEnergy)
    println("best = " + bestConfig)
    println("best energy = " + bestConfig.getEnergy)
    list
  }
  def posteriorMean(list: Seq[CoalConfiguration], file: File) {
    val start = System.currentTimeMillis
    val writer = new PrintWriter(file)
    val parList = list.par
    (0 to L).map(x => {
      val s = parList.map { c => c.getHeight(x) }.sum / updates
      writer.write("" + s + "\n")
    })
    writer.close
    val end = System.currentTimeMillis
    println("posterior mean computed in " + (end - start) + " ms (" + (end - start) / 1000 + " s)")
  }
}

object CoalApp extends App {
  val y = Source.fromFile("./src/main/resources/coal_green.csv").getLines.map(x => x.toInt).toSeq
  //  val parameters = new Parameters("./src/main/resources/coal_parameters.xml")
  //  val L = parameters.getInt("L")
  //  val Kmax = parameters.getInt("kmax")
  //  val lambda = parameters.getDouble("lambda")
  //  val alpha = parameters.getDouble("alpha")
  //  val beta = parameters.getDouble("beta")
  //  val burnin = parameters.getInt("burnin")
  //  val updates = parameters.getInt("updates")
  //  val app = new CoalApp(y, L, Kmax, lambda, alpha, beta, burnin, updates)
  val app = new CoalApp(y, 40907, 30, 3, 1, 200, 4000, 20000)
  implicit val rng = new MersenneTwister(21)
  val l = app.apply
  app.posteriorMean(l.toSeq, new File("result.txt"))
}