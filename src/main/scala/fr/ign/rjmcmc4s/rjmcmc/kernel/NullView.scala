package fr.ign.rjmcmc4s.rjmcmc.kernel

import fr.ign.rjmcmc4s.configuration.Configuration
import scala.collection.Iterable
import scala.collection.mutable.MutableList
import fr.ign.rjmcmc4s.configuration.Modification

object NullView extends View {
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]) = 1.
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]) = 1.
  def dimension = 0
}