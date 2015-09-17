package fr.ign.rjmcmc4s.rjmcmc.kernel

class IdentityTransform(val size:Int) extends Transform {
  def apply(direct: Boolean, input: Iterable[Double]): TransformResult = {
    new TransformResult(input, 1.)
  }
}