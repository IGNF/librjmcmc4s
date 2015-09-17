package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.Iterable
import fr.ign.rjmcmc4s.rjmcmc.kernel.Transform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformResult
import org.apache.commons.math3.util.FastMath

class HeightTransform extends Transform {
  def apply(direct: Boolean, input: Iterable[Double]) = {
    val iterator = input.iterator
    val h1 = iterator.next
    val j = iterator.next
    val u = iterator.next - 0.5 // Should be in [-0.5, 0.5]
    val e = FastMath.exp(u)
    var output = List(h1 * e, j, 0.5-u)
    new TransformResult(output, e)
    //abs(determinant(jacobian([h*%e^u,-u],[h,u]))) = %e^u
  }
  def size = 3
}