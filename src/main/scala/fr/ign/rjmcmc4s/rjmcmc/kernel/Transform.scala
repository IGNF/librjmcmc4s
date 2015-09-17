package fr.ign.rjmcmc4s.rjmcmc.kernel

class TransformResult(val output: Iterable[Double], val jacob: Double)
/**
 * An inversible transform.
 * @author Julien Perret
 */
trait Transform {
  /**
   * Apply the transform on the input vector.
   * @param direct if <code>true</code> apply the transform, else apply its inverse
   * @param input the input vector
   * @return the transformed vector and the determinant of the jacobian
   */
  def apply(direct: Boolean, input: Iterable[Double]): TransformResult
  /**
   * @return the size of the transform
   */
  def size: Int
}