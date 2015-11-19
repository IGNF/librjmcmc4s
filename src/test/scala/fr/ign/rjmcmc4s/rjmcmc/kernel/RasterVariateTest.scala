package fr.ign.rjmcmc4s.rjmcmc.kernel

import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.mutable.ListBuffer

object RasterVariateTest extends App {
  val iter = 1000000
  val value = IndexedSeq[Int](
    1, 2, 3, 2, 1,
    0, 3, 3, 3, 0,
    1, 2, 3, 2, 1,
    5, 2, 0, 2, 1,
    1, 2, 3, 2, 1,
    6, 2, 7, 2, 9)
  val size = Seq[Int](5, 6)
  implicit val rng = new MersenneTwister(1)
  val variate = new RasterVariate[Int](value, size)

  var count = ArrayBuffer.fill(size(0) * size(1))(0)
  for (i <- 0 until iter) {
    val result = variate.compute(2)
    val value = result._1
    val pdf = result._2
    if (pdf == 0) println("sampling failed")
    if (pdf != variate.pdf(value)) println("pdf mismatch : " + pdf + "=" + variate.pdf(value))
    val x = (value(0) * size(0)).toInt
    val y = (value(1) * size(1)).toInt
    count(x + y * size(0)) += 1
  }

  val displayValues = false
  val displayCounts = false
  if (displayValues) {
    for (y <- 0 until size(1)) {
      for (x <- 0 until size(0)) {
        print(value(x + y * size(0)).toDouble + " ")
      }
      println
    }
    println
  }
  if (displayCounts) {
    println("counts for visual comparison")
    val sum = count.sum / value.sum
    for (y <- 0 until size(1)) {
      for (x <- 0 until size(0)) {
        print(count(x + y * size(0)).toDouble / sum + " ")
      }
      println
    }
    println
  }
  val countsum = count.sum
  val valuesum = value.sum
//  var apd = 0.
  var nbZeros = 0
  var apdBuffer = ListBuffer[Double]()
  for (i <- 0 until variate.m_totsize) {
    val ratio = value(i).toDouble / valuesum.toDouble
    if (ratio > 0) {
      val apd = Math.abs((count(i).toDouble / countsum.toDouble - ratio) / ratio)
      apdBuffer+=apd
    } else {
      nbZeros += 1
    }
  }
//  apd /= variate.m_totsize
//  println("absolute percentage difference = " + (apd * 100) + " %")
  println("absolute percentage difference = " + (apdBuffer.min * 100) + "% " + (apdBuffer.sum / apdBuffer.size * 100)+"% " + (apdBuffer.max * 100) + "%")
  println(nbZeros + " zeros")
}