package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.Iterable
import fr.ign.rjmcmc4s.rjmcmc.kernel.Transform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformResult
import org.apache.commons.math3.util.FastMath

class PositionTransform extends Transform {
  def apply(direct: Boolean, input: Iterable[Double]) = {
    val iterator = input.iterator
    val s0 = iterator.next
    val s1 = iterator.next
    val s2 = iterator.next
    val u = iterator.next
    var output = List(s0, s0 + u * (s2 - s0), s2, (s1 - s0) / (s2 - s0))
    new TransformResult(output, 1.0)
    // det(determinant(jacobian([s0, s0+u*(s2-s0), s2, (s1 - s0) / (s2 - s0)],[s0,s1,s2,u]))) = 1
  }
  def size = 4
}