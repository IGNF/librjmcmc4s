package fr.ign.rjmcmc4s.samples.coal.kernel

import scala.collection.Iterable

import org.apache.commons.math3.util.FastMath

import fr.ign.rjmcmc4s.rjmcmc.kernel.Transform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformResult

class BirthDeathTransform extends Transform {
  def apply(direct: Boolean, input: Iterable[Double]) = {
    val iterator = input.iterator
    if (direct) { //BIRTH
      val h0 = iterator.next
      val s0 = iterator.next
      val s1 = iterator.next
      val ss = iterator.next
      val u = iterator.next
      val v = (1 - u) / u
      val _h0 = h0 * FastMath.pow(v, ((ss - s1) / (s1 - s0)))
      val _h1 = h0 * FastMath.pow(v, ((ss - s0) / (s1 - s0)))
      var output = List(_h0, _h1, s0, ss, s1)
      new TransformResult(output, FastMath.pow(_h0 + _h1, 2) / h0)
      // rat(determinant(jacobian([h0*((1-u)/u)^((ss-s1) / (s1-s0)), h0*((1-u)/u)^((ss-s0) / (s1-s0)), s0, ss, s1],[h0,s0,s1,ss,u])));
      // = (h0*((1-u)/u)^((ss-s0)/(s1-s0))*((1-u)/u)^((ss-s1)/(s1-s0)))/(u^2-u)
      // = (h'0 + h'1)^2 / h0           
    } else { //DEATH
      val h0 = iterator.next
      val h1 = iterator.next
      val s0 = iterator.next
      val s1 = iterator.next
      val s2 = iterator.next
      val e0 = (s1 - s0) / (s2 - s0)
      val e1 = (s2 - s1) / (s2 - s0)
      val hs = FastMath.pow(h0, e0) * FastMath.pow(h1, e1)
      val u = h0 / (h0 + h1)
      var output = List(hs, s0, s2, s1, u)
      new TransformResult(output, hs / FastMath.pow(h0 + h1, 2))
      // rat(determinant(jacobian([h0^((s1-s0) / (s2-s0)) * h1^((s2-s1) / (s2-s0)), s0, s2, s1, h0/(h0+h1)],[h0,h1,s0,s1,s2])));
      // = (h0^((s1-s0)/(s2-s0))*h1^((s2-s1)/(s2-s0)))/(h1^2+2*h0*h1+h0^2)
      // = h'0 / (h0+h1)^2
    }
  }
  def size = 5
}