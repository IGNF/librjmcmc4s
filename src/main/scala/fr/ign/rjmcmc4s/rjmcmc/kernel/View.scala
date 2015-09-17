package fr.ign.rjmcmc4s.rjmcmc.kernel

import fr.ign.rjmcmc4s.configuration.Configuration
import fr.ign.rjmcmc4s.configuration.Modification
import scala.collection.mutable.MutableList

object View {
  case class ViewSamplingResult(m: Modification, output: Seq[Double], p: Double)
  case class ViewInverseSamplingResult(m: Modification, p: Double)
}
trait View {
  def pdf(c: Configuration, m: Modification, output: MutableList[Double]): Double
  def inversePdf(c: Configuration, m: Modification, input: Iterable[Double]): Double
  def dimension: Int
}