package fr.ign.rjmcmc4s.rjmcmc.kernel

import scala.collection.Iterable
import Array._

class DiagonalAffineTransform extends Transform {
  var mat: Array[Array[Double]] = null
  var delta: Array[Array[Double]] = null
  var absJacobian: Array[Double] = ofDim[Double](2)
  var size: Int = 0
  def this(d: Iterable[Double], v: Iterable[Double]) = {
    this()
    size = d.size
    mat = ofDim[Double](2, size)
    delta = ofDim[Double](2, size)
    var det = 1.
    var i = 0
    val itD = d.iterator
    val itV = v.iterator
    while (itD.hasNext && itV.hasNext) {
      val valD = itD.next
      val valV = itV.next
      det *= valD
      mat(0)(i) = valD
      mat(1)(i) = 1. / valD
      delta(0)(i) = valV
      delta(1)(i) = -valV / valD
      i += 1
    }
    absJacobian(0) = det.abs
    absJacobian(1) = (1. / det).abs;
  }
  def apply(direct: Boolean, input: Iterable[Double]) = {
    val i = if (direct) 0 else 1
    val m = mat(i)
    val d = delta(i)
    var index = 0
    val output = input.map(x => {
      val r = x * m(index) + d(index)
      index += 1
      r
    })
    new TransformResult(output, absJacobian(i))
  }
}