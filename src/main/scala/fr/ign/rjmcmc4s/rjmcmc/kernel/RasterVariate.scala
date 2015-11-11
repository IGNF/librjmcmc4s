package fr.ign.rjmcmc4s.rjmcmc.kernel

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.traversable2ops

import org.apache.commons.math3.random.RandomGenerator

/**
 * This variate generates a point in [0,1]^N with a piecewise uniform density where the pieces are given by a uniform rectangular voxel grid.
 * <code>size</code> gives the number of elements of the grid in each dimension.
 * <code>pdf</code> gives the unnormalized pdf values of each voxel as a N dimensional array of size (size).
 */
class RasterVariate[T <% Double](val rng: RandomGenerator)(pdf: Seq[T], val m_size: Seq[Int]) extends Variate {
  val N = m_size.size
  val m_totsize = m_size.product
  val m_cdf = buildCdf(m_totsize, pdf, m_size).toParArray
  val m_sum = pdf.foldLeft(0.0)((a, b) => a + b)
  val m_rand = new org.apache.commons.math3.distribution.UniformRealDistribution(rng, 0.0, 1.0)

  def buildCdf(totsize: Int, pdf: Seq[T], size: Seq[Int]) = {
//    println("buildcdf " + totsize)
    var sum = 0.0
    var cdf = ArrayBuffer(0.0)
    var start = System.currentTimeMillis()
    for (i <- 0 until totsize) {
      //      if (i % 10000 == 0) {
      //        val end = System.currentTimeMillis()
      //        println("i " + i + " => " + (end - start) + " ms")
      //        start = end
      //      }
      sum = sum + pdf(i)
      cdf.append(sum)
    }
//    println(cdf.size)
//    println("sum = " + sum)
    cdf = cdf.map(_ / sum)

    //    var s = 0.
    //    var i = 0
    //    var start = System.currentTimeMillis()
    //    val cd = 0. +: pdf.map(v => {
    //      s += v
    //          if (i % 10000 == 0) {
    //            val end = System.currentTimeMillis()
    //            println("i " + i + " => " + (end - start) + " ms")
    //            start = end
    //          }
    //      i += 1
    //      s
    //    })
    //    println("i = " + i + " / " + pdf.size)
    //    println("sum = " + s)
    //    val cdd = cd.map(_ / s)
//    println("cdf built")
    //    cdf.toList
    cdf.toIndexedSeq
  }
  override def compute(size: Int): (Seq[Double], Double) = {
    //    var start = System.currentTimeMillis
    val x = m_rand.sample
    var offset = m_cdf.indexWhere(p => p > x) - 1
    //    var end = System.currentTimeMillis
    //    println("offset " + (end - start))
    //    start = end
    //    println("x=" + x + " => offset = " + offset)
    val pdf = (m_cdf(offset + 1) - m_cdf(offset)) * m_totsize
    //    end = System.currentTimeMillis
    //    println("pdf " + (end - start))
    //    start = end
    var output = ArrayBuffer.fill(N)(0.0)
    for (i <- 0 until N) {
      val ix = offset % m_size(i)
      output(i) = (ix + m_rand.sample) / m_size(i)
      offset /= m_size(i)
    }
    //    end = System.currentTimeMillis
    //    println("vals " + (end - start))
    //    start = end
    (output, pdf)
  }
  /**
   * Returns the continuous probability of the variate sampling, arguments are constant.
   * @param var1
   *        a variate
   * @return the continuous probability of the variate sampling, arguments are constant.
   */
  override def pdf(it: Iterable[Double]) = {
    val offset = getOffset(it)
    (m_cdf(offset + 1) - m_cdf(offset)) * m_totsize
  }

  def getOffset(it: Iterable[Double]) = {
    var offset = 0
    var stride = 1
    val iterator = it.iterator
    for (i <- 0 until N) {
      val x = iterator.next
      if (x < 0.0 || x > 1.0) 0.0
      val ix = (x * m_size(i)).toInt
      offset += stride * ix
      stride *= m_size(i)
    }
    offset
  }
}