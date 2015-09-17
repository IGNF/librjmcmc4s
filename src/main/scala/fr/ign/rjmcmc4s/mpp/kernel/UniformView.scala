package fr.ign.rjmcmc4s.mpp.kernel

import scala.collection.Iterator
import scala.collection.mutable.MutableList

import org.apache.commons.math3.distribution.UniformIntegerDistribution
import org.apache.commons.math3.random.RandomGenerator

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import fr.ign.rjmcmc4s.mpp.configuration.BirthDeathModification
import fr.ign.rjmcmc4s.mpp.configuration.ListBufferConfiguration
import fr.ign.rjmcmc4s.rjmcmc.kernel.View

trait ObjectBuilder[T] {
  def dimension: Int
  def create(it: Iterable[Double]): T
}

class UniformView[T <: Iterable[Double]](val rng: RandomGenerator, val n: Int, val builder: ObjectBuilder[T]) extends View {
  val dimension = builder.dimension
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]) = (c, m) match {
    case (configuration: ListBufferConfiguration[T], modification: BirthDeathModification[T]) =>
      modification.death.clear
      def sample(numberOfSamples: Int, size: Int, previous: List[Int], denom: Double): Double = numberOfSamples match {
        case 0 => denom
        case _ => {
          val v = size match {
            case 1 => 0
            case _ => new UniformIntegerDistribution(rng, 0, size - 1).sample
          }
          val shifted = previous.foldLeft(v)((x, y) => if (x >= y) x + 1 else x)
          val t = configuration.apply(shifted)
          modification.insertDeath(t)
          output ++= t
          sample(numberOfSamples - 1, size - 1, previous ++ List(shifted), denom * size)
        }
      }
      1. / sample(n, configuration.size, Nil, 1.)
    case _ => 0.
  }
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = (c, m) match {
    case (configuration: ListBufferConfiguration[T], modification: BirthDeathModification[T]) =>
      modification.birth.clear
      val begin = configuration.size - modification.death.size + 1
      val denom = (begin until begin + n).foldLeft(1.)((x, y) => {
        val t = this.builder.create(input)
//        val t = T(input)
        modification.insertBirth(t)
        x * y
      })
      1. / denom
    case _ => 0.
  }
}